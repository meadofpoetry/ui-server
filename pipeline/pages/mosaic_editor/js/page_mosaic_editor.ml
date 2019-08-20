open Js_of_ocaml
open Netlib
open Components
open Pipeline_http_js

let ( >>= ) x f = Lwt_result.(map_err Api_js.Http.error_to_string @@ x >>= f)

let () =
  let open React in
  let (scaffold : Scaffold.t) = Js.Unsafe.global##.scaffold in
  let thread =
    Http_wm.get_layout ()
    >>= fun wm -> Http_structure.get_annotated ()
    >>= fun streams ->
    Api_js.Websocket.JSON.open_socket ~path:(Uri.Path.Format.of_string "ws") ()
    >>= fun socket -> Http_wm.Event.get socket
    >>= fun (_, wm_event) -> Http_structure.Event.get_annotated socket
    >>= fun (_, streams_event) ->
    let editor = Container_editor.make ~scaffold streams wm in
    let notif =
      E.merge (fun _ -> editor#notify) ()
        [ E.map (fun x -> `Layout x) wm_event
        ; E.map (fun x -> `Streams x) streams_event
        ] in
    editor#set_on_destroy (fun () ->
        E.stop ~strong:true notif;
        E.stop ~strong:true wm_event;
        E.stop ~strong:true streams_event;
        Api_js.Websocket.close_socket socket);
    Lwt.return_ok editor in
  let loader = Ui_templates.Loader.make_widget_loader thread in
  Element.add_class loader "wm";
  scaffold#set_body loader
