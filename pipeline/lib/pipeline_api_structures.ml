open Containers
open Pipeline_protocol
open Pipeline_api_common
open Api.Interaction
open Api.Interaction.Json
open Common

let filter_by_input (inputs : Topology.topo_input list)
      (streams : Structure.Streams.t) =
  match inputs with
  | [] -> streams
  | inputs ->
     List.filter (fun ({ source; _ } : Structure.t) ->
         match Stream.get_input source with
         | None -> false
         | Some input ->
            List.mem ~eq:Topology.equal_topo_input input inputs)
       streams

let filter_by_stream_id (ids : Stream.ID.t list)
      (streams : Structure.Streams.t) =
  match ids with
  | [] -> streams
  | ids ->
     List.filter (fun ({ source; _ } : Structure.t) ->
         List.mem ~eq:Stream.ID.equal source.id ids)
       streams

let filter_by_analyzed (to_be_analyzed : bool option)
      (streams : Structure.Streams.t) =
  let open Structure in
  match to_be_analyzed with
  | None -> streams
  | Some to_be_analyzed ->
     List.filter_map (fun (({ structure; _ } as t): t) ->
         let ({ channels; _ } : structure) = structure in
         let channels =
           List.filter_map (fun (c : channel) ->
               let pids =
                 List.filter (fun (p : pid) ->
                     Bool.equal to_be_analyzed p.to_be_analyzed)
                   c.pids in
               match pids with
               | [] -> None
               | pids -> Some { c with pids }) channels in
         match channels with
         | [] -> None
         | channels ->
            let structure = { structure with channels } in
            Some { t with structure }) streams

module WS = struct

  open React

  let get_event api ids inputs analyzed =
    let event = S.changes api.notifs.streams in
    match ids, inputs, analyzed with
    | [], [], None -> event
    | _ ->
       E.map (filter_by_input inputs
              % filter_by_stream_id ids
              % filter_by_analyzed analyzed) event
       |> E.fmap (function [] -> None | l -> Some l)

  let get (api : api) ids inputs analyzed _ body sock_data () =
    let open Structure in
    let event = get_event api ids inputs analyzed in
    Api.Socket.handler socket_table sock_data event
      Structure.Streams.to_yojson body

  let get_streams (api : api) ids inputs analyzed _ body sock_data () =
    let open Structure in
    let event = get_event api ids inputs analyzed in
    let event = React.E.map (List.map (fun (x : t) -> x.source)) event in
    Api.Socket.handler socket_table sock_data event
      (Json.List.to_yojson Stream.to_yojson) body

end

module HTTP = struct

  let set (api : api) headers body () =
    of_body body >>= fun js ->
    match Structure.Streams.of_yojson js with
    | Error e -> respond_error e ()
    | Ok x ->
       api.requests.streams.set x
       >>= function Ok () -> respond_result_unit (Ok ())

  let get (api : api) ids inputs analyzed headers body () =
    api.requests.streams.get ()
    >|= (function
         | Error e -> Error (Json.String.to_yojson e)
         | Ok v ->
            let v =
              filter_by_input inputs v
              |> filter_by_stream_id ids
              |> filter_by_analyzed analyzed in
            Ok (Structure.Streams.to_yojson v))
    >>= respond_result

  let get_streams (api : api) ids inputs analyzed headers body () =
    let open Structure in
    api.requests.streams.get ()
    >|= (function
         | Error e -> Error (Json.String.to_yojson e)
         | Ok v ->
            let v =
              filter_by_input inputs v
              |> filter_by_stream_id ids
              |> filter_by_analyzed analyzed
              |> List.map (fun (x : t) -> x.source) in
            Ok ((Json.List.to_yojson Stream.to_yojson) v))
    >>= respond_result

end

let handler (api : api) =
  let open Api_handler in
  create_dispatcher "structure"
    [ create_ws_handler ~docstring:"Structure websocket"
        ~path:Uri.Path.Format.empty
        ~query:Uri.Query.[ "id", (module List(Stream.ID))
                         ; "input", (module List(Topology.Show_topo_input))
                         ; "analyzed", (module Option(Bool)) ]
        (WS.get api)
    ; create_ws_handler ~docstring:"Streams websocket"
        ~path:Uri.Path.Format.("streams" @/ empty)
        ~query:Uri.Query.[ "id", (module List(Stream.ID))
                         ; "input", (module List(Topology.Show_topo_input))
                         ; "analyzed", (module Option(Bool)) ]
        (WS.get_streams api)
    ]
    [ `GET, [ create_handler ~docstring:"Structure"
                ~path:Uri.Path.Format.empty
                ~query:Uri.Query.[ "id", (module List(Stream.ID))
                                 ; "input", (module List(Topology.Show_topo_input))
                                 ; "analyzed", (module Option(Bool)) ]
                (HTTP.get api)
            ; create_handler ~docstring:"Streams"
                ~path:Uri.Path.Format.("streams" @/ empty)
                ~query:Uri.Query.[ "id", (module List(Stream.ID))
                                 ; "input", (module List(Topology.Show_topo_input))
                                 ; "analyzed", (module Option(Bool)) ]
                (HTTP.get_streams api)
            ]
    ; `POST, [ create_handler ~docstring:"Post structure"
                 ~restrict:[`Guest]
                 ~path:Uri.Path.Format.empty
                 ~query:Uri.Query.empty
                 (HTTP.set api)
             ]
    ]
