open Js_of_ocaml
open Netlib
open Components
open Pipeline_types
open Pipeline_http_js

let ( >>= ) x f = Lwt_result.(map_err Api_js.Http.error_to_string @@ x >>= f)

module Test = struct
  let make_widget ?(type_ = Wm.Video)
      ?(domain = Wm.Nihil)
      ?aspect
      ~x ~y ~w ~h () : string * Wm.widget =
    let position =
      Some { Wm.
             left = x
           ; top = y
           ; right = x + w
           ; bottom = y + h
           } in
    string_of_int @@ Random.bits (),
    { position
    ; description = "Sample widget"
    ; pid = Some 4096
    ; type_
    ; aspect
    ; domain
    ; layer = 0
    }

  let make_container
      ?(title = "Sample container")
      ?(widgets = [])
      ~position () : string * Wm.container =
    title, { position; widgets }

  let widgets =
    [ make_widget ~type_:Audio ~x:0 ~y:0 ~w:50 ~h:50 ()
    ; make_widget ~aspect:(16, 9) ~x:50 ~y:0 ~w:50 ~h:50 ()
    ; make_widget
        ~domain:(Chan { stream = Application_types.Stream.ID.make "id"
                      ; channel = 2
                      })
        ~x:0 ~y:50 ~w:50 ~h:50 ()
    ]

  let containers =
    [ make_container
        ~title:"Россия 1"
        ~position:{ left = 0; top = 0; right = 240; bottom = 160 }
        ~widgets
        ()
    ; make_container
        ~title:"ТНТ"
        ~position:{ left = 240; top = 0; right = 760; bottom = 160 }
        ~widgets
        ()
    ; make_container
        ~title:"Канал"
        ~position:{ left = 760; top = 0; right = 1280; bottom = 360 }
        ~widgets
        ()
    ; make_container
        ~title:"Первый канал"
        ~position:{ left = 0; top = 160; right = 760; bottom = 720 }
        ~widgets
        ()
    ; make_container
        ~title:"СТС"
        ~position:{ left = 760; top = 360; right = 1280; bottom = 720 }
        ~widgets
        ()
    ]

  let (wm : Wm.t) =
    { layout = (* containers *) []
    ; widgets = widgets
    ; resolution = 1280, 720
    }

end

let () =
  let open React in
  let scaffold = Scaffold.attach (Dom_html.getElementById "root") in
  let thread =
    (* Lwt.return_ok Test.wm *)
    Http_wm.get_layout ()
    >>= fun wm ->
    (* let wm = { wm with widgets = Test.widgets } in *)
    Http_structure.get_streams_applied_with_source ()
    >>= fun streams ->
    Api_js.Websocket.JSON.open_socket ~path:(Uri.Path.Format.of_string "ws") ()
    >>= fun socket -> Http_wm.Event.get socket
    >>= fun (_, wm_event) -> Http_structure.Event.get_streams_applied_with_source socket
    >>= fun (_, streams_event) ->
    let editor = Container_editor.make ~scaffold streams wm in
    let notif =
      E.merge (fun _ -> editor#notify) ()
        [ E.map (fun x -> `Layout x) wm_event
        ; E.map (fun x -> `Streams x) streams_event
        ] in
    editor#set_on_destroy (fun () ->
        React.E.stop ~strong:true notif;
        React.E.stop ~strong:true wm_event;
        React.E.stop ~strong:true streams_event;
        Api_js.Websocket.close_socket socket);
    Lwt.return_ok editor in
  let body = Ui_templates.Loader.create_widget_loader thread in
  body#add_class "wm";
  scaffold#set_body body
