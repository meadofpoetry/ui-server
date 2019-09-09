open Application_types
open Netlib.Uri
open Components_tyxml
open Components_lab_tyxml
module Api_http = Api_cohttp.Make (User) (Body)
module Api_template = Api_cohttp_template.Make (User)
module Api_websocket = Api_websocket.Make (User) (Body) (Body_ws)

let make_icon ?classes path =
  let icon = Icon.Markup.SVG.create ?classes ~d:path () in
  Tyxml.Html.toelt icon

let make_overflow_menu actions =
  let actions =
    List.map
      (fun (href, id, icon, name) ->
        let ( ^:: ) x l =
          match x with
          | None -> l
          | Some x -> x :: l
        in
        let icon_class = Components_tyxml.Icon_button.CSS.icon in
        let icon = Tyxml.Html.tot @@ make_icon ~classes:[icon_class] icon in
        let attrs = Tyxml.Html.(Option.map a_id id ^:: [a_title name]) in
        let classes = [Components_tyxml.Top_app_bar.CSS.action_item] in
        match href with
        | None -> Icon_button.Markup.create ~icon ~classes ~attrs ()
        | Some href -> Icon_button.Markup.create_anchor ~href ~icon ~classes ~attrs ())
      actions
  in
  Overflow_menu.Markup.create ~actions ()

let make_top_app_bar_row () =
  Tyxml.Html.toelt @@ Top_app_bar.Markup.create_row ~sections:[] ()

let pages () : Api_template.topmost Api_template.item list =
  let open Api_template in
  let video_path = "/mosaic/video" in
  let editor_path = "/mosaic/editor" in
  let video_page_overflow_menu =
    make_overflow_menu
      [ Some editor_path, None, Svg_icons.pencil, "Редактировать"
      ; None, Some "wizard", Svg_icons.auto_fix, "Мастер"
      ; None, Some "hotkeys", Svg_icons.keyboard, "Горячие клавиши" ]
  in
  let video_page_props =
    make_template_props
      ~title:"Мозаика"
      ~side_sheet:(make_side_sheet_props ~clipped:false ())
      ~top_app_bar_content:
        [ Tyxml.Html.toelt
          @@ Top_app_bar.Markup.create_section
               ~align:`End
               ~children:[video_page_overflow_menu]
               () ]
      ~pre_scripts:[`Src "/js/adapter.min.js"]
      ~post_scripts:[`Src "/js/page-mosaic-video.js"]
      ~stylesheets:["/css/page-mosaic-video.min.css"]
      ~content:[Tyxml.Html.toelt @@ Mosaic_video_template.make_player ()]
      ()
  in
  let editor_page_props =
    make_template_props
      ~title:"Редактор мозаики"
      ~top_app_bar_content:
        (List.map
           Tyxml.Html.toelt
           Top_app_bar.Markup.[create_section ~align:`End ~children:[] ()])
      ~side_sheet:(make_side_sheet_props ~clipped:true ~typ:`Dismissible ())
      ~pre_scripts:[`Src "/js/ResizeObserver.js"]
      ~post_scripts:[`Src "/js/page-mosaic-editor.js"]
      ~stylesheets:["/css/page-mosaic-editor.min.css"]
      ~content:[]
      ()
  in
  simple
    ~priority:(`Index 2)
    ~title:"Мозаика"
    ~icon:(make_icon Components_tyxml.Svg_icons.view_grid)
    ~path:(Path.of_string video_path)
    video_page_props
  @ simple
      ~priority:(`Index 3)
      ~title:"Редактор мозаики"
      ~icon:(make_icon Components_tyxml.Svg_icons.pencil)
      ~path:(Path.of_string editor_path)
      editor_page_props

(* TODO remove state *)
let handlers (state : Pipeline_protocol.Protocol.state) =
  let open Pipeline_protocol in
  let open Api_http in
  [ merge
      ~prefix:"pipeline"
      [ make
          ~prefix:"structures"
          [ node
              ~doc:"Annotated structures"
              ~meth:`GET
              ~path:Path.Format.("annotated" @/ empty)
              ~query:Query.empty
              (Pipeline_api.get_annotated state)
          ; node
              ~doc:"Applied structs"
              ~meth:`GET
              ~path:Path.Format.("applied" @/ empty)
              ~query:
                Query.
                  [ "id", (module List (Stream.ID))
                  ; "input", (module List (Topology.Show_topo_input)) ]
              (Pipeline_api.get_applied_structures state)
          ; node
              ~doc:"Apply structs"
              ~meth:`POST
              ~restrict:[`Guest]
              ~path:Path.Format.("apply" @/ empty)
              ~query:Query.empty
              (Pipeline_api.apply_structures state) ]
      ; make
          ~prefix:"wm"
          [ node
              ~doc:"Wm"
              ~meth:`GET
              ~path:Path.Format.empty
              ~query:Query.empty
              (Pipeline_api.get_wm_layout state)
          ; node
              ~doc:"Post wm"
              ~restrict:[`Guest]
              ~meth:`POST
              ~path:Path.Format.empty
              ~query:Query.empty
              (Pipeline_api.apply_wm_layout state) ]
      ; make
          ~prefix:"settings"
          [ node
              ~doc:"Settings"
              ~meth:`GET
              ~path:Path.Format.empty
              ~query:Query.empty
              (Pipeline_api.get_settings state)
          ; node
              ~doc:"Post settings"
              ~meth:`POST
              ~restrict:[`Guest]
              ~path:Path.Format.empty
              ~query:Query.empty
              (Pipeline_api.apply_settings state) ]
      ; make
          ~prefix:"status"
          [ node
              ~doc:"Status"
              ~meth:`GET
              ~path:Path.Format.empty
              ~query:Query.["id", (module List (Stream.ID))]
              (Pipeline_api.get_status state) ]
      ; make
          ~prefix:"history"
          [ node
              ~doc:"Streams archive"
              ~meth:`GET
              ~path:Path.Format.("streams/archive" @/ empty)
              ~query:
                Query.
                  [ "limit", (module Option (Int))
                  ; "from", (module Option (Time_uri.Show))
                  ; "to", (module Option (Time_uri.Show))
                  ; "duration", (module Option (Time_uri.Show_relative)) ]
              (Pipeline_api_history.get_streams state)
          ; node
              ~doc:"Structure archive"
              ~meth:`GET
              ~path:Path.Format.("structure/archive" @/ empty)
              ~query:
                Query.
                  [ "uris", (module List (Netlib.Uri))
                  ; "limit", (module Option (Int))
                  ; "from", (module Option (Time_uri.Show))
                  ; "to", (module Option (Time_uri.Show))
                  ; "duration", (module Option (Time_uri.Show_relative)) ]
              (Pipeline_api_history.get_structures state) ] ] ]

let ws (state : Pipeline_protocol.Protocol.state) =
  let open Pipeline_protocol in
  let open Api_websocket in
  (* TODO add closing event *)
  (*  let socket_table = make_socket_table () in
  state.cleanup#set_cb (fun () -> close_sockets socket_table); *)
  [ merge
      ~prefix:"pipeline"
      [ make
          ~prefix:"structures"
          [ event_node
              ~doc:"Annotated websocket"
              ~path:Path.Format.("annotated" @/ empty)
              ~query:Query.empty
              (Pipeline_api.Event.get_annotated state)
          ; event_node
              ~doc:"Applied structures websocket"
              ~path:Path.Format.("applied" @/ empty)
              ~query:
                Query.
                  [ "id", (module List (Stream.ID))
                  ; "input", (module List (Topology.Show_topo_input)) ]
              (Pipeline_api.Event.get_applied_structures state) ]
      ; make
          ~prefix:"wm"
          [ event_node
              ~doc:"WM socket"
              ~path:Path.Format.empty
              ~query:Query.empty
              (Pipeline_api.Event.get_wm_layout state) ]
      ; make
          ~prefix:"settings"
          [ event_node
              ~doc:"Settings socket"
              ~path:Path.Format.empty
              ~query:Query.empty
              (Pipeline_api.Event.get_settings state) ]
      ; make
          ~prefix:"status"
          [ event_node
              ~doc:"Stream status socket"
              ~path:Path.Format.empty
              ~query:Query.["id", (module List (Stream.ID))]
              (Pipeline_api.Event.get_status state) ]
      ; make
          ~prefix:"measurements"
          [ event_node
              ~doc:"Video data socket"
              ~path:Path.Format.("video" @/ empty)
              ~query:
                Query.
                  [ "stream", (module Option (Stream.ID))
                  ; "channel", (module Option (Int))
                  ; "pid", (module Option (Int)) ]
              (Pipeline_api_measurements.Event.get_video state)
          ; event_node
              ~doc:"Audio data socket"
              ~path:Path.Format.("audio" @/ empty)
              ~query:
                Query.
                  [ "stream", (module Option (Stream.ID))
                  ; "channel", (module Option (Int))
                  ; "pid", (module Option (Int)) ]
              (Pipeline_api_measurements.Event.get_audio state) ] ] ]
