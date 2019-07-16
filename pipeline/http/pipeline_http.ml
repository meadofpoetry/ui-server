open Application_types
open Netlib.Uri

module Api_http = Api_cohttp.Make(User)(Body)

module Api_template = Api_cohttp_template.Make(User)

module Api_websocket = Api_websocket.Make(User)(Body)(Body_ws)

module Icon = Components_tyxml.Icon.Make(Tyxml.Xml)(Tyxml.Svg)(Tyxml.Html)

module Button = Components_tyxml.Button.Make(Tyxml.Xml)(Tyxml.Svg)(Tyxml.Html)

module Icon_button = Components_tyxml.Icon_button.Make(Tyxml.Xml)(Tyxml.Svg)(Tyxml.Html)

module Top_app_bar = Components_tyxml.Top_app_bar.Make(Tyxml.Xml)(Tyxml.Svg)(Tyxml.Html)

let make_icon ?classes path =
  let open Icon.SVG in
  let path = create_path path () in
  let icon = create ?classes [path] () in
  Tyxml.Html.toelt icon

let make_anchor_buttons ?href ~class_ ~icon ~label () =
  let icon = Tyxml_html.tot icon in
  let compact = Components_tyxml.BEM.add_modifier class_ "compact" in
  let full = Components_tyxml.BEM.add_modifier class_ "full" in
  let button = Button.create_anchor
      ~classes:[class_; full] ?href ~icon ~label () in
  let icon_button = Icon_button.create_anchor
      ~classes:[class_; compact] ?href ~icon () in
  List.map Tyxml.Html.toelt [button; icon_button]

let make_top_app_bar_row () =
  Tyxml.Html.toelt
  @@ Top_app_bar.create_row ~sections:[] ()

let pages () : Api_template.topmost Api_template.item list =
  let open Api_template in
  (* let side_sheet_toggle = Mosaic_video_template.(
   *     make_icon_button
   *       ~classes:[CSS.side_sheet_icon]
   *       Components_tyxml.Svg_icons.tune) in *)
  let menu_toggle = Mosaic_video_template.(
      make_icon_button
        ~classes:[CSS.menu_icon]
        Components_tyxml.Svg_icons.dots_vertical) in
  let video_path = "/mosaic/video" in
  let editor_path = "/mosaic/editor" in
  let video_page_props =
    make_template_props
      ~title:"Мозаика"
      ~side_sheet:(make_side_sheet_props ~clipped:false ())
      ~top_app_bar_actions:(make_anchor_buttons
                              ~href:editor_path
                              ~icon:(make_icon
                                       ~classes:[Components_tyxml.Button.CSS.icon]
                                       Components_tyxml.Svg_icons.pencil)
                              ~class_:Mosaic_video_template.CSS.edit
                              ~label:"Редактировать" ()
                            @ [Tyxml.Html.toelt menu_toggle])
      ~pre_scripts:[Src "/js/adapter.min.js"]
      ~post_scripts:[Src "/js/mosaic_video.js"]
      ~stylesheets:["/css/mosaic_video.min.css"]
      ~content:[Tyxml.Html.toelt @@ Mosaic_video_template.make_player ()]
      ()
  in
  let editor_page_props =
    make_template_props
      ~title:"Редактор мозаики"
      ~side_sheet:(make_side_sheet_props
                     ~clipped:true
                     ~typ:`Dismissible
                     ())
      ~pre_scripts:[Src "/js/ResizeObserver.js"]
      ~post_scripts:[Src "/js/mosaic_editor.js"]
      ~stylesheets:["/css/mosaic_editor.min.css"]
      ~content:[]
      ()
  in
  simple ~priority:(`Index 2)
    ~title:"Мозаика"
    ~icon:(make_icon Components_tyxml.Svg_icons.view_grid)
    ~path:(Path.of_string video_path)
    video_page_props
  @ simple ~priority:(`Index 3)
    ~title:"Редактор мозаики"
    ~icon:(make_icon Components_tyxml.Svg_icons.pencil)
    ~path:(Path.of_string editor_path)
    editor_page_props

(* TODO remove state *)
let handlers
      (state : Pipeline_protocol.Protocol.state) =
  let open Pipeline_protocol in
  let open Api_http in
  [ merge ~prefix:"pipeline"
      [ make ~prefix:"structures"
          [ node ~doc:"Annotated structures"
              ~meth:`GET
              ~path:Path.Format.("annotated" @/ empty)
              ~query:Query.empty
              (Pipeline_api.get_annotated state)
          ; node ~doc:"Applied structs"
              ~meth:`GET
              ~path:Path.Format.("applied" @/ empty)
              ~query:Query.[ "id", (module List(Stream.ID))
                           ; "input", (module List(Topology.Show_topo_input)) ]
              (Pipeline_api.get_applied_structures state)
          ; node ~doc:"Apply structs"
              ~meth:`POST
              ~restrict:[`Guest]
              ~path:Path.Format.("apply" @/ empty)
              ~query:Query.empty
              (Pipeline_api.apply_structures state)
          ]
      ; make ~prefix:"wm"
          [ node ~doc:"Wm"
              ~meth:`GET
              ~path:Path.Format.empty
              ~query:Query.empty
              (Pipeline_api.get_wm_layout state)
          ; node ~doc:"Post wm"
              ~restrict:[`Guest]
              ~meth:`POST
              ~path:Path.Format.empty
              ~query:Query.empty
              (Pipeline_api.apply_wm_layout state)
          ]
      ; make ~prefix:"settings"
          [ node ~doc:"Settings"
              ~meth:`GET
              ~path:Path.Format.empty
              ~query:Query.empty
              (Pipeline_api.get_settings state)
          ; node ~doc:"Post settings"
              ~meth:`POST
              ~restrict:[`Guest]
              ~path:Path.Format.empty
              ~query:Query.empty
              (Pipeline_api.apply_settings state)
          ]
      ; make ~prefix:"status"
          [ node ~doc:"Status"
              ~meth:`GET
              ~path:Path.Format.empty
              ~query:Query.["id", (module List(Stream.ID))]
              (Pipeline_api.get_status state)
          ]
      ; make ~prefix:"history"
          [ node ~doc:"Streams archive"
              ~meth:`GET
              ~path:Path.Format.("streams/archive" @/ empty)
              ~query:Query.[ "limit",    (module Option(Int))
                           ; "from",     (module Option(Time_uri.Show))
                           ; "to",       (module Option(Time_uri.Show))
                           ; "duration", (module Option(Time_uri.Show_relative)) ]
              (Pipeline_api_history.get_streams state)
          ; node ~doc:"Structure archive"
              ~meth:`GET
              ~path:Path.Format.("structure/archive" @/ empty)
              ~query:Query.[ "uris",     (module List(Netlib.Uri))
                           ; "limit",    (module Option(Int))
                           ; "from",     (module Option(Time_uri.Show))
                           ; "to",       (module Option(Time_uri.Show))
                           ; "duration", (module Option(Time_uri.Show_relative)) ]
              (Pipeline_api_history.get_structures state)
          ]
      ]
  ]

let ws (state : Pipeline_protocol.Protocol.state) =
  let open Pipeline_protocol in
  let open Api_websocket in
  (* TODO add closing event *)
(*  let socket_table = make_socket_table () in
  state.cleanup#set_cb (fun () -> close_sockets socket_table); *)

  [ merge ~prefix:"pipeline"
      [ make ~prefix:"structures"
          [ event_node ~doc:"Annotated websocket"
              ~path:Path.Format.("annotated" @/ empty)
              ~query:Query.empty
              (Pipeline_api.Event.get_annotated state)
          ; event_node ~doc:"Applied structures websocket"
              ~path:Path.Format.("applied" @/ empty)
              ~query:Query.[ "id",    (module List(Stream.ID))
                           ; "input", (module List(Topology.Show_topo_input)) ]
              (Pipeline_api.Event.get_applied_structures state)
          ]
      ; make ~prefix:"wm"
          [ event_node ~doc:"WM socket"
              ~path:Path.Format.empty
              ~query:Query.empty
              (Pipeline_api.Event.get_wm_layout state)
          ]
      ; make ~prefix:"settings"
          [ event_node ~doc:"Settings socket"
              ~path:Path.Format.empty
              ~query:Query.empty
              (Pipeline_api.Event.get_settings state)
          ]
      ; make ~prefix:"status"
          [ event_node ~doc:"Stream status socket"
              ~path:Path.Format.empty
              ~query:Query.["id", (module List(Stream.ID))]
              (Pipeline_api.Event.get_status state)
          ]
      ; make ~prefix:"measurements"
          [ event_node ~doc:"Video data socket"
              ~path:Path.Format.("video" @/ empty)
              ~query:Query.[ "stream", (module Option(Stream.ID))
                           ; "channel", (module Option(Int))
                           ; "pid", (module Option(Int)) ]
              (Pipeline_api_measurements.Event.get_video state)
          ; event_node ~doc:"Audio data socket"
              ~path:Path.Format.("audio" @/ empty)
              ~query:Query.[ "stream", (module Option(Stream.ID))
                           ; "channel", (module Option(Int))
                           ; "pid", (module Option(Int)) ]
              (Pipeline_api_measurements.Event.get_audio state)
          ]
      ]
  ]
