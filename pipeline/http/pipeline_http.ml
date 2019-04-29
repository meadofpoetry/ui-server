open Application_types
open Netlib.Uri

module Api_http = Api_cohttp.Make(User)(Body)

module Api_template = Api_cohttp_template.Make(User)

module Api_websocket = Api_websocket.Make(User)(Body)

module Icon = Components_markup.Icon.Make(Tyxml.Xml)(Tyxml.Svg)(Tyxml.Html)

let make_icon path =
  let open Icon.SVG in
  let path = create_path path () in
  let icon = create [path] () in
  Tyxml.Html.toelt icon

let pages () : Api_template.topmost Api_template.item list =
  let open Api_template in
  let props = { title = Some "Мозаика"
              ; pre_scripts = [ Src "/js/janus.nojquery.js"
                              ; Src "/js/adapter.min.js"
                              ]
              ; post_scripts = [Src "/js/page_mosaic.js"]
              ; stylesheets = ["/css/pipeline.min.css"]
              ; content = []
              }
  in
  simple ~priority:(`Index 1)
    ~title:"Мозаика"
    ~icon:(make_icon Icon.SVG.Path.collage)
    ~path:(Path.of_string "pipeline")
    props

let handlers (api : Pipeline_protocol.Protocol.api) =
  let open Pipeline_protocol in
  let open Api_http in
  [ merge ~prefix:"pipeline"
      [ make ~prefix:"wm"
          [ node ~doc:"Wm"
              ~meth:`GET
              ~path:Path.Format.empty
              ~query:Query.empty
              (Pipeline_api.get_wm_layout api)
          ; node ~doc:"Post wm"
              ~restrict:[`Guest]
              ~meth:`POST
              ~path:Path.Format.empty
              ~query:Query.empty
              (Pipeline_api.apply_wm_layout api)
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
              (Pipeline_api.get_status api)
          ]
      ; make ~prefix:"streams"
          [ node ~doc:"Streams"
              ~meth:`GET
              ~path:Path.Format.empty
              ~query:Query.[ "id", (module List(Stream.ID))
                           ; "input", (module List(Topology.Show_topo_input))  ]
              (Pipeline_api_structures.get_streams api)
          ; node ~doc:"Applied streams"
              ~meth:`GET
              ~path:Path.Format.("applied" @/ empty)
              ~query:Query.[ "id", (module List(Stream.ID))
                           ; "input", (module List(Topology.Show_topo_input)) ]
              (Pipeline_api_structures.get_streams_applied api)
          ; node ~doc:"Streams with source"
              ~meth:`GET
              ~path:Path.Format.("with_source" @/ empty)
              ~query:Query.[ "id", (module List(Stream.ID))
                               ; "input", (module List(Topology.Show_topo_input))  ]
              (Pipeline_api_structures.get_streams_with_source api)
          ; node ~doc:"Applied streams with source"
              ~meth:`GET
              ~path:Path.Format.("applied_with_source" @/ empty)
              ~query:Query.[ "id", (module List(Stream.ID))
                           ; "input", (module List(Topology.Show_topo_input)) ]
              (Pipeline_api_structures.get_streams_applied_with_source api)
          ; node ~doc:"Apply streams"
              ~restrict:[`Guest]
              ~meth:`POST
              ~path:Path.Format.empty
              ~query:Query.empty
              (Pipeline_api_structures.apply_streams api)
          ]
      ; make ~prefix:"history"
          [ node ~doc:"Streams archive"
              ~meth:`GET
              ~path:Path.Format.("streams/archive" @/ empty)
              ~query:Query.[ "limit",    (module Option(Int))
                           ; "from",     (module Option(Time_uri.Show))
                           ; "to",       (module Option(Time_uri.Show))
                           ; "duration", (module Option(Time_uri.Show_relative)) ]
              (Pipeline_api_history.get_streams api)
          ; node ~doc:"Structure archive"
              ~meth:`GET
              ~path:Path.Format.("structure/archive" @/ empty)
              ~query:Query.[ "uris",     (module List(Netlib.Uri))
                           ; "limit",    (module Option(Int))
                           ; "from",     (module Option(Time_uri.Show))
                           ; "to",       (module Option(Time_uri.Show))
                           ; "duration", (module Option(Time_uri.Show_relative)) ]
              (Pipeline_api_history.get_structures api)
          ]
      ]
  ]

let ws (api : Pipeline_protocol.Protocol.api) =
  let open Pipeline_protocol in
  let open Api_http in
  let open Api_websocket in
  [ merge ~prefix:"pipeline"
      [ make ~prefix:"wm"
          [ node ~doc:"WM socket"
              ~path:Path.Format.empty
              ~query:Query.empty
              (Pipeline_api.Event.get_wm_layout api)
          ]
      (*; make ~prefix:"settings"
      ~docstring:"Settings socket"
        ~path:Uri.Path.Format.empty
        ~query:Uri.Query.empty
        (WS.get_settings api) *)
      ; make ~prefix:"status"
          [ node ~doc:"Stream status socket"
              ~path:Path.Format.empty
              ~query:Query.["id", (module List(Stream.ID))]
              (Pipeline_api.Event.get_status api)
          ]
      ; make ~prefix:"streams"
          [ node ~doc:"Streams websocket"
              ~path:Path.Format.empty
              ~query:Query.[ "id", (module List(Stream.ID))
                           ; "input", (module List(Topology.Show_topo_input)) ]
              (Pipeline_api_structures.Event.get_streams api)
          ; node ~doc:"Applied streams websocket"
              ~path:Path.Format.("applied" @/ empty)
              ~query:Query.[ "id",    (module List(Stream.ID))
                           ; "input", (module List(Topology.Show_topo_input)) ]
              (Pipeline_api_structures.Event.get_applied api)
          ; node ~doc:"Streams with source websocket"
              ~path:Path.Format.("with_source" @/ empty)
              ~query:Query.[ "id",    (module List(Stream.ID))
                           ; "input", (module List(Topology.Show_topo_input)) ]
              (Pipeline_api_structures.Event.get_streams_packed api)
          ; node ~doc:"Applied streams with source websocket"
              ~path:Path.Format.("applied_with_source" @/ empty)
              ~query:Query.[ "id",    (module List(Stream.ID))
                           ; "input", (module List(Topology.Show_topo_input)) ]
              (Pipeline_api_structures.Event.get_streams_packed api)
          ]
      ; make ~prefix:"measurements"
          [ node ~doc:"Video data socket"
              ~path:Path.Format.("video" @/ empty)
              ~query:Query.[ "stream", (module Option(Stream.ID))
                           ; "channel", (module Option(Int))
                           ; "pid", (module Option(Int)) ]
              (Pipeline_api_measurements.Event.get_video api)
          ; node ~doc:"Audio data socket"
              ~path:Path.Format.("audio" @/ empty)
              ~query:Query.[ "stream", (module Option(Stream.ID))
                           ; "channel", (module Option(Int))
                           ; "pid", (module Option(Int)) ]
              (Pipeline_api_measurements.Event.get_audio api)
          ]
      ]
  ]
