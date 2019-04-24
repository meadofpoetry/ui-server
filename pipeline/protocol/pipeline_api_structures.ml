open Pipeline_types
open Application_types
open Lwt.Infix

let (%) f g x = f (g x)
   
let filter_by_input (inputs : Topology.topo_input list)
      (streams : (Netlib.Uri.t * Stream.t) list) =
  match inputs with
  | [] -> streams
  | inputs ->
     List.filter (fun (_, stream) ->
         match Stream.get_input stream with
         | None -> false
         | Some input ->
            List.exists (Topology.equal_topo_input input) inputs)
       streams

let filter_by_stream_id (ids : Stream.ID.t list)
      (streams : (Netlib.Uri.t * Stream.t) list) =
  match ids with
  | [] -> streams
  | ids ->
     List.filter (fun (_, (stream : Stream.t)) ->
         List.exists (Stream.ID.equal stream.id) ids)
       streams

let uris = List.map fst

let filter_by_uris (uris : Netlib.Uri.t list)
    : Structure.t list -> Structure.t list =
  List.filter (fun (stream : Structure.t) ->
      List.exists (fun uri -> Netlib.Uri.equal uri stream.uri) uris)

module Event = struct

  open Util_react

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

  let get_streams (api : Protocol.api) ids inputs _user _body _env state =
    let event =
      filter_event ids inputs !(api.sources)
        (S.changes api.notifs.streams)
      |> E.map (Util_json.List.to_yojson Structure.to_yojson)
    in
    Lwt.return (`Ev (state, event))

  let get_applied (api : Protocol.api) ids inputs _user _body _env state =
    let event = filter_event ids inputs !(api.sources)
                  (S.changes api.notifs.applied_structs)
                |> E.map (Util_json.List.to_yojson Structure.to_yojson)
    in
    Lwt.return (`Ev (state, event))
    
  let get_streams_packed (api : Protocol.api) ids inputs _user _body _env state =
    let event = filter_map_event ids inputs
                  (Structure_conv.match_streams api.sources)
                  !(api.sources)
                  (S.changes api.notifs.streams)
                |> E.map (Util_json.List.to_yojson Structure.packed_to_yojson)
    in
    Lwt.return (`Ev (state, event))
    
  let get_applied_packed (api : Protocol.api) ids inputs _user _body _env state =
    let event = filter_map_event ids inputs
                  (Structure_conv.match_streams api.sources)
                  !(api.sources)
                  (S.changes api.notifs.applied_structs)
                |> E.map (Util_json.List.to_yojson Structure.packed_to_yojson)
    in
    Lwt.return (`Ev (state, event))
    
end

let filter_data ids inputs streams data =
  match ids, inputs with
  | [], [] -> data
  | _ ->
     let uris = streams
                |> filter_by_stream_id ids
                |> filter_by_input inputs
                |> uris
     in filter_by_uris uris data
      
let get_streams (state : Protocol.state) (api : Protocol.api) ids inputs _user _body _env _state =
  match state.backend with
  | None -> Lwt.return (`Error "not ready")
  | Some backend ->
     Protocol.Qoe_backend.Stream_parser.get_structure backend
     |> Lwt_result.map (Util_json.List.to_yojson Structure.to_yojson
                        % filter_data ids inputs !(api.sources))
     >>= function
     | Ok v -> Lwt.return (`Value v)
     | Error (`Qoe_backend e) -> Lwt.return (`Error e)

let get_streams_applied (state : Protocol.state) (api : Protocol.api) ids inputs _user _body _env _state =
  match state.backend with
  | None -> Lwt.return (`Error "not ready")
  | Some backend ->
     Protocol.Qoe_backend.Graph.get_structure backend
     |> Lwt_result.map (Util_json.List.to_yojson Structure.to_yojson
                        % filter_data ids inputs !(api.sources))
     >>= function
     | Ok v -> Lwt.return (`Value v)
     | Error (`Qoe_backend e) -> Lwt.return (`Error e)

let get_streams_with_source (state : Protocol.state) (api : Protocol.api) ids inputs _user _body _env _state =
  match state.backend with
  | None -> Lwt.return (`Error "not ready")
  | Some backend ->
     Protocol.Qoe_backend.Stream_parser.get_structure backend
     |> Lwt_result.map (Util_json.List.to_yojson Structure.packed_to_yojson
                        % Structure_conv.match_streams api.sources
                        % filter_data ids inputs !(api.sources))
     >>= function
     | Ok v -> Lwt.return (`Value v)
     | Error (`Qoe_backend e) -> Lwt.return (`Error e)

let get_streams_applied_with_source (state : Protocol.state) (api : Protocol.api) ids inputs _user _body _env _state =
  match state.backend with
  | None -> Lwt.return (`Error "not ready")
  | Some backend ->
     Protocol.Qoe_backend.Graph.get_structure backend
     |> Lwt_result.map (Util_json.List.to_yojson Structure.packed_to_yojson
                        % Structure_conv.match_streams api.sources
                        % filter_data ids inputs !(api.sources))
     >>= function
     | Ok v -> Lwt.return (`Value v)
     | Error (`Qoe_backend e) -> Lwt.return (`Error e)

let apply_streams (state : Protocol.state) (api : Protocol.api) _user body _env _state =
  match Util_json.List.of_yojson Structure.of_yojson body with
  | Error e -> Lwt.return (`Error e)
  | Ok x ->
     match state.backend with
     | None -> Lwt.return (`Error "not ready")
     | Some backend ->
        Protocol.Qoe_backend.Graph.apply_structure backend x
        >>= function
        | Error (`Qoe_backend e) -> Lwt.return (`Error  e)
        | Ok () ->
           api.options.structures#set x
           >>= fun () ->
           Lwt.return `Unit
                               
