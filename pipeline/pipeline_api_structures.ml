(*
open Containers
open Pipeline_protocol
open Pipeline_api_common
open Api.Interaction
open Api.Interaction.Json
open Common

let filter_by_input (inputs : Topology.topo_input list)
      (streams : (Url.t * Stream.t) list) =
  match inputs with
  | [] -> streams
  | inputs ->
     List.filter (fun (_, stream) ->
         match Stream.get_input stream with
         | None -> false
         | Some input ->
            List.mem ~eq:Topology.equal_topo_input input inputs)
       streams

let filter_by_stream_id (ids : Stream.ID.t list)
      (streams : (Url.t * Stream.t) list) =
  match ids with
  | [] -> streams
  | ids ->
     List.filter (fun (_, (stream : Stream.t)) ->
         List.mem ~eq:Stream.ID.equal stream.id ids)
       streams

let uris = List.map fst

let filter_by_uris (uris : Url.t list)
    : Structure.t list -> Structure.t list =
  List.filter (fun (stream : Structure.t) ->
      List.exists (fun uri -> Url.equal uri stream.uri) uris)
    
module WS = struct

  open React

  let filter_event ids inputs streams event =
    match ids, inputs with
    | [], [] -> event
    | _ ->
       let uris = streams
                  |> filter_by_stream_id ids
                  |> filter_by_input inputs
                  |> uris
       in E.map (filter_by_uris uris) event

  let filter_map_event ids inputs f streams event =
    match ids, inputs with
    | [], [] -> E.map f event
    | _ ->
       let uris = streams
                  |> filter_by_stream_id ids
                  |> filter_by_input inputs
                  |> uris
       in E.map (f % filter_by_uris uris) event

  let get_streams (api : api) ids inputs _ body sock_data () =
    let open Structure in
    let event = filter_event ids inputs !(api.sources)
                  (S.changes api.notifs.streams) in
    Api.Socket.handler socket_table sock_data event
      (Json.List.to_yojson to_yojson) body

  let get_applied (api : api) ids inputs _ body sock_data () =
    let open Structure in
    let event = filter_event ids inputs !(api.sources)
                  (S.changes api.notifs.applied_structs) in
    Api.Socket.handler socket_table sock_data event
      (Json.List.to_yojson to_yojson) body

  let get_streams_packed (api : api) ids inputs _ body sock_data () =
    let open Structure in
    let event = filter_map_event ids inputs
                  (Structure_conv.match_streams api.sources)
                  !(api.sources)
                  (S.changes api.notifs.streams) in
    Api.Socket.handler socket_table sock_data event
      (Json.List.to_yojson packed_to_yojson) body

  let get_applied_packed (api : api) ids inputs _ body sock_data () =
    let open Structure in
    let event = filter_map_event ids inputs
                  (Structure_conv.match_streams api.sources)
                  !(api.sources)
                  (S.changes api.notifs.applied_structs) in
    Api.Socket.handler socket_table sock_data event
      (Json.List.to_yojson packed_to_yojson) body

end

module HTTP = struct

  let filter_data ids inputs streams data =
    match ids, inputs with
    | [], [] -> data
    | _ ->
       let uris = streams
                  |> filter_by_stream_id ids
                  |> filter_by_input inputs
                  |> uris
       in filter_by_uris uris data
  
  let get_streams (api : api) ids inputs _headers _body () =
    Message.Protocol.stream_parser_get api.channel ()
    |> Lwt_result.map (Json.List.to_yojson Structure.to_yojson
                       % filter_data ids inputs !(api.sources))
    |> Lwt_result.map_err (fun x -> `String x)
    >>= respond_result

  let get_streams_applied (api : api) ids inputs _headers _body () =
    Message.Protocol.graph_get_structure api.channel ()
    |> Lwt_result.map (Json.List.to_yojson Structure.to_yojson
                       % filter_data ids inputs !(api.sources))
    |> Lwt_result.map_err (fun x -> `String x)
    >>= respond_result

  let get_streams_with_source (api : api) ids inputs _headers _body () =
    Message.Protocol.stream_parser_get api.channel ()
    |> Lwt_result.map (Json.List.to_yojson Structure.packed_to_yojson
                       % Structure_conv.match_streams api.sources
                       % filter_data ids inputs !(api.sources))
    |> Lwt_result.map_err (fun x -> `String x)
    >>= respond_result

  let get_streams_applied_with_source (api : api) ids inputs _headers _body () =
    Message.Protocol.graph_get_structure api.channel ()
    |> Lwt_result.map (Json.List.to_yojson Structure.packed_to_yojson
                       % Structure_conv.match_streams api.sources
                       % filter_data ids inputs !(api.sources))
    |> Lwt_result.map_err (fun x -> `String x)
    >>= respond_result

  let apply_streams (api : api) _headers body () =
    of_body body >>= fun js ->
    match Json.List.of_yojson Structure.of_yojson js with
    | Error e -> respond_error e ()
    | Ok x ->
       Message.Protocol.graph_apply_structure
         ~options:api.options.structures api.channel x
       >>= function Ok () -> respond_result_unit (Ok ())
                  | Error e -> respond_error e ()
    
end

let handler (api : api) =
  let open Api_handler in
  create_dispatcher "streams"
    [ create_ws_handler ~docstring:"Streams websocket"
        ~path:Uri.Path.Format.empty
        ~query:Uri.Query.[ "id", (module List(Stream.ID))
                         ; "input", (module List(Topology.Show_topo_input)) ]
        (WS.get_streams api)
    ; create_ws_handler ~docstring:"Applied streams websocket"
        ~path:Uri.Path.Format.("applied" @/ empty)
        ~query:Uri.Query.[ "id",    (module List(Stream.ID))
                         ; "input", (module List(Topology.Show_topo_input)) ]
        (WS.get_applied api)
    ; create_ws_handler ~docstring:"Streams with source websocket"
        ~path:Uri.Path.Format.("with_source" @/ empty)
        ~query:Uri.Query.[ "id",    (module List(Stream.ID))
                         ; "input", (module List(Topology.Show_topo_input)) ]
        (WS.get_streams_packed api)
    ; create_ws_handler ~docstring:"Applied streams with source websocket"
        ~path:Uri.Path.Format.("applied_with_source" @/ empty)
        ~query:Uri.Query.[ "id",    (module List(Stream.ID))
                         ; "input", (module List(Topology.Show_topo_input)) ]
        (WS.get_streams_packed api)
    ]
    [ `GET, [ create_handler ~docstring:"Streams"
                ~path:Uri.Path.Format.empty
                ~query:Uri.Query.[ "id", (module List(Stream.ID))
                                 ; "input", (module List(Topology.Show_topo_input))  ]
                (HTTP.get_streams api)
            ; create_handler ~docstring:"Applied streams"
                ~path:Uri.Path.Format.("applied" @/ empty)
                ~query:Uri.Query.[ "id", (module List(Stream.ID))
                                 ; "input", (module List(Topology.Show_topo_input)) ]
                (HTTP.get_streams_applied api)
            ; create_handler ~docstring:"Streams with source"
                ~path:Uri.Path.Format.("with_source" @/ empty)
                ~query:Uri.Query.[ "id", (module List(Stream.ID))
                                 ; "input", (module List(Topology.Show_topo_input))  ]
                (HTTP.get_streams_with_source api)
            ; create_handler ~docstring:"Applied streams with source"
                ~path:Uri.Path.Format.("applied_with_source" @/ empty)
                ~query:Uri.Query.[ "id", (module List(Stream.ID))
                                 ; "input", (module List(Topology.Show_topo_input)) ]
                (HTTP.get_streams_applied_with_source api)
            ]
    ; `POST, [ create_handler ~docstring:"Apply streams"
                 ~restrict:[`Guest]
                 ~path:Uri.Path.Format.empty
                 ~query:Uri.Query.empty
                 (HTTP.apply_streams api)
             ]
    ]
 *)
