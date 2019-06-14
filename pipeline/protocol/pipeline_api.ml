open Application_types
open Pipeline_types
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

let filter_data ids inputs streams data =
  match ids, inputs with
  | [], [] -> data
  | _ ->
     let uris = streams
                |> filter_by_stream_id ids
                |> filter_by_input inputs
                |> uris
     in filter_by_uris uris data

module Event = struct

  open Protocol
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

  let get_annotated (state : Protocol.state) _user =
    let signal =
      React.S.l3 (fun active avail stored ->
          Structure.Annotated.annotate ~active ~avail ~stored)
        state.notifs.applied_structs
        state.notifs.streams
        state.options.structures#s
    in
    let event =
      (S.changes signal)
      |> E.map Structure.Annotated.to_yojson
    in
    Lwt.return event
     
  let get_applied_structures (state : Protocol.state) ids inputs _user =
    let event =
      S.changes state.notifs.applied_structs
      |> filter_event ids inputs (state.sources)
      |> E.map Structure.Many.to_yojson
    in
    Lwt.return event

  let get_settings (state : state) _user =
    let event =
      S.changes state.options.settings#s
      |> E.map Settings.to_yojson
    in
    Lwt.return event

  let get_wm_layout (state : Protocol.state) _user =
    let signal =
      React.S.l2 (fun active stored ->
          Wm.Annotated.annotate ~active ~stored)
        state.notifs.wm
        state.options.wm#s
    in
    let event =
      S.changes signal
      |> E.map Wm.Annotated.to_yojson
    in
    Lwt.return event

  let get_status (state : Protocol.state) ids _user =
    let event = S.changes state.notifs.status in
    let event = match ids with
      | [] -> event
      | ids ->
         React.E.map (List.filter (fun (x : Qoe_status.t) ->
                          List.exists (Application_types.Stream.ID.equal x.stream) ids))
           event
    in
    let event = E.map (Util_json.List.to_yojson Qoe_status.to_yojson) event in
    Lwt.return event

end
             
let set body conv apply =
  match conv body with
  | Error e -> Lwt.return (`Error e)
  | Ok x ->
     apply x
     >>= function Ok () -> Lwt.return `Unit
                | Error (`Qoe_backend e) -> Lwt.return (`Error e) (* TODO respond result *)

let get_annotated (state : Protocol.state) _user _body _env _state =
  match state.backend with
  | None -> Lwt.return (`Error "not ready")
  | Some backend ->
     Protocol.Qoe_backend.Stream_parser.get_structure backend
     >>= fun avail ->
     Protocol.Qoe_backend.Graph.get_structure backend
     >>= fun active ->
     state.options.structures#get
     >>= fun stored ->
     match active, avail with
     | Ok active, Ok avail ->
        let annotated = Structure.Annotated.annotate ~active ~avail ~stored in
        Lwt.return (`Value (Structure.Annotated.to_yojson annotated))
     | Error (`Qoe_backend e), _ -> Lwt.return (`Error e)
     | _, Error (`Qoe_backend e) -> Lwt.return (`Error e)

let get_applied_structures (state : Protocol.state) ids inputs _user _body _env _state =
  match state.backend with
  | None -> Lwt.return (`Error "not ready")
  | Some backend ->
     Protocol.Qoe_backend.Graph.get_structure backend
     |> Lwt_result.map (Structure.Many.to_yojson
                        % filter_data ids inputs state.sources)
     >>= function
     | Ok v -> Lwt.return (`Value v)
     | Error (`Qoe_backend e) -> Lwt.return (`Error e)

let apply_structures (state : Protocol.state) _user body _env _state =
  match Structure.Many.of_yojson body with
  | Error e -> Lwt.return (`Error e)
  | Ok x ->
     match state.backend with
     | None -> Lwt.return (`Error "not ready")
     | Some backend ->
        Protocol.Qoe_backend.Graph.apply_structure backend x
        >>= function
        | Error (`Qoe_backend e) -> Lwt.return (`Error  e)
        | Ok () ->
           state.options.structures#set x
           >>= fun () ->
           Lwt.return `Unit
                                          
let apply_settings (state : Protocol.state) _user body _env _state =
  let (>>=) = Lwt_result.bind in
  set body Settings.of_yojson
    Protocol.(fun x ->
    match state.backend with
    | None -> Lwt.return_error (`Qoe_backend "not ready")
    | Some backend ->
       Qoe_backend.Analysis_settings.apply_settings backend x
       >>= fun () ->
       Lwt_result.ok @@ state.options.settings#set x)

let get_settings (state : Protocol.state) _user _body _env _state =
  match state.backend with
  | None -> Lwt.return (`Error "not ready")
  | Some backend ->
     Protocol.Qoe_backend.Analysis_settings.get_settings backend
     >>= (function
          | Error (`Qoe_backend e) -> Lwt.return (`Error e)
          | Ok v -> Lwt.return (`Value (Settings.to_yojson v)))

let apply_wm_layout (state : Protocol.state) _user body _env _state =
  let (>>=) = Lwt_result.bind in
  set body Wm.of_yojson
    Protocol.(fun x ->
    match state.backend with
    | None -> Lwt.return_error (`Qoe_backend "not ready")
    | Some backend ->
       Qoe_backend.Mosaic.apply_layout backend x
       >>= fun () ->
       Lwt_result.ok @@ state.options.wm#set x)

let get_wm_layout (state : Protocol.state) _user _body _env _state =
  match state.backend with
  | None -> Lwt.return (`Error "not ready")
  | Some backend ->
     Protocol.Qoe_backend.Mosaic.get_layout backend
     >>= fun active ->
     state.options.wm#get
     >>= fun stored ->
     match active with
     | Error (`Qoe_backend e) -> Lwt.return (`Error e)
     | Ok v -> Lwt.return (`Value (Wm.to_yojson v))

let get_status (state : Protocol.state) ids _user _body _env _state =
  React.S.value state.notifs.status
  |> (fun l ->
    match ids with
    | [] -> l
    | ids -> List.filter (fun (x : Qoe_status.t) ->
                 List.exists (Application_types.Stream.ID.equal x.stream) ids) l)
  |> Util_json.List.to_yojson Qoe_status.to_yojson
  |> fun r -> Lwt.return (`Value r)


(*
let handlers (state : state) =
  [ State_handler.add_layer "pipeline"
      ; Pipeline_api_measurements.handler api
      ; Pipeline_api_history.handler api
      ]
  ]
 *)
