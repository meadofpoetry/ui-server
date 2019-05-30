open Application_types
open Netlib.Uri

module Api_http = Api_cohttp.Make(User)(Body)

module Api_template = Api_cohttp_template.Make(User)

module Api_websocket = Api_websocket.Make(User)(Body)(Api_websocket.Json_msg)

module Icon = Components_tyxml.Icon.Make(Tyxml.Xml)(Tyxml.Svg)(Tyxml.Html)

let make_icon path =
  let open Icon.SVG in
  let path = create_path path () in
  let icon = create [path] () in
  Tyxml.Html.toelt icon

let pages () : Api_template.topmost Api_template.item list =
  let open Api_template in
  let side_sheet_toggle = Mosaic_video_template.(
      make_icon_button
        ~classes:[CSS.side_sheet_icon]
        Components_tyxml.Svg_icons.tune) in
  let menu_toggle = Mosaic_video_template.(
      make_icon_button
        ~classes:[CSS.menu_icon]
        Components_tyxml.Svg_icons.dots_horizontal) in
  let props =
    make_template_props
      ~title:"Мозаика"
      ~side_sheet:(make_side_sheet_props ~clipped:false ())
      ~top_app_bar_actions:[ Tyxml.Html.toelt menu_toggle
                           ; Tyxml.Html.toelt side_sheet_toggle ]
      ~pre_scripts:[Src "/js/adapter.min.js"]
      ~post_scripts:[Src "/js/mosaic_video.js"]
      ~stylesheets:[ "/css/pipeline.min.css"
                   ; "/css/mosaic_video.min.css" ]
      ~content:[Tyxml.Html.toelt @@ Mosaic_video_template.make_player ()]
      ()
  in
  simple ~priority:(`Index 1)
    ~title:"Мозаика"
    ~icon:(make_icon Components_tyxml.Svg_icons.collage)
    ~path:(Path.of_string "pipeline")
    props

(* TODO remove state *)
let handlers
      (state : Pipeline_protocol.Protocol.state) =
  let open Pipeline_protocol in
  let open Api_http in
  [ merge ~prefix:"pipeline"
      [ make ~prefix:"wm"
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
     (* ; make ~prefix:"settings"
             [ `GET, [ create_handler ~docstring:"Settings"
                 ~path:Uri.Path.Format.empty
                 ~query:Uri.Query.empty
                 (HTTP.get_settings api)
                 ]
             ; `POST, [ create_handler ~docstring:"Post settings"
                 ~restrict:[`Guest]
                 ~path:Uri.Path.Format.empty
                 ~query:Uri.Query.empty
                 (HTTP.set_settings api)
             ]*)
      ; make ~prefix:"status"
          [ node ~doc:"Status"
              ~meth:`GET
              ~path:Path.Format.empty
              ~query:Query.["id", (module List(Stream.ID))]
              (Pipeline_api.get_status state)
          ]
      ; make ~prefix:"streams"
          [ node ~doc:"Streams"
              ~meth:`GET
              ~path:Path.Format.empty
              ~query:Query.[ "id", (module List(Stream.ID))
                           ; "input", (module List(Topology.Show_topo_input))  ]
              (Pipeline_api_structures.get_streams state)
          ; node ~doc:"Applied streams"
              ~meth:`GET
              ~path:Path.Format.("applied" @/ empty)
              ~query:Query.[ "id", (module List(Stream.ID))
                           ; "input", (module List(Topology.Show_topo_input)) ]
              (Pipeline_api_structures.get_streams_applied state)
          ; node ~doc:"Streams with source"
              ~meth:`GET
              ~path:Path.Format.("with-source" @/ empty)
              ~query:Query.[ "id", (module List(Stream.ID))
                           ; "input", (module List(Topology.Show_topo_input))  ]
              (Pipeline_api_structures.get_streams_with_source state)
          ; node ~doc:"Applied streams with source"
              ~meth:`GET
              ~path:Path.Format.("applied-with-source" @/ empty)
              ~query:Query.[ "id", (module List(Stream.ID))
                           ; "input", (module List(Topology.Show_topo_input)) ]
              (Pipeline_api_structures.get_streams_applied_with_source state)
          ; node ~doc:"Apply streams"
              ~restrict:[`Guest]
              ~meth:`POST
              ~path:Path.Format.empty
              ~query:Query.empty
              (Pipeline_api_structures.apply_streams state)
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
      [ make ~prefix:"wm"
          [ event_node ~doc:"WM socket"
              ~path:Path.Format.empty
              ~query:Query.empty
              (Pipeline_api.Event.get_wm_layout state)
          ]
      (*; make ~prefix:"settings"
      ~docstring:"Settings socket"
        ~path:Uri.Path.Format.empty
        ~query:Uri.Query.empty
        (WS.get_settings api) *)
      ; make ~prefix:"status"
          [ event_node ~doc:"Stream status socket"
              ~path:Path.Format.empty
              ~query:Query.["id", (module List(Stream.ID))]
              (Pipeline_api.Event.get_status state)
          ]
      ; make ~prefix:"streams"
          [ event_node ~doc:"Streams websocket"
              ~path:Path.Format.empty
              ~query:Query.[ "id", (module List(Stream.ID))
                           ; "input", (module List(Topology.Show_topo_input)) ]
              (Pipeline_api_structures.Event.get_streams state)
          ; event_node ~doc:"Applied streams websocket"
              ~path:Path.Format.("applied" @/ empty)
              ~query:Query.[ "id",    (module List(Stream.ID))
                           ; "input", (module List(Topology.Show_topo_input)) ]
              (Pipeline_api_structures.Event.get_applied state)
          ; event_node ~doc:"Streams with source websocket"
              ~path:Path.Format.("with-source" @/ empty)
              ~query:Query.[ "id",    (module List(Stream.ID))
                           ; "input", (module List(Topology.Show_topo_input)) ]
              (Pipeline_api_structures.Event.get_streams_packed state)
          ; event_node ~doc:"Applied streams with source websocket"
              ~path:Path.Format.("applied-with-source" @/ empty)
              ~query:Query.[ "id",    (module List(Stream.ID))
                           ; "input", (module List(Topology.Show_topo_input)) ]
              (Pipeline_api_structures.Event.get_applied_packed state)
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
