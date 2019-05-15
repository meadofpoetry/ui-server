open Board_niitv_dvb_types
open Application_types

let ( >>= ) = Lwt.bind

let ( % ) f g x = f (g x)

let find_stream_by_receiver_id ~source_id id streams =
  let multi_id = Stream.Multi_TS_ID.make
      ~source_id
      ~stream_id:id in
  Stream.find_by_multi_id multi_id streams

let find_receiver_by_stream_id id streams =
  let id =
    List.find_opt (fun (s : Stream.t) -> Stream.ID.equal id s.id)
    @@ React.S.value streams in
  match id with
  | None -> None
  | Some id ->
    try
      let mid = Stream.to_multi_id id in
      Some (Stream.Multi_TS_ID.stream_id mid)
    with Failure _ -> None

module Event = struct
  open Util_react

  let list_to_option = function [] -> None | l -> Some l

  let mem id = List.exists (Stream.ID.equal id)

  let filter_if_needed ids event =
    match ids with
    | [] -> event
    | ids -> E.fmap (list_to_option % List.filter (fun (id, _) -> mem id ids)) event

  let to_json f (v : (Stream.ID.t * 'a ts) list) =
    Util_json.(
      List.to_yojson (Pair.to_yojson Stream.ID.to_yojson (ts_to_yojson f)) v)

  let map_event (source_id : int)
      (streams : Stream.t list signal)
      (event : (int * 'a) list event) =
    S.sample (fun (data : (int * 'a) list)
               (streams : Stream.t list) ->
               Boards.Util.List.filter_map (fun (id, x) ->
                   match find_stream_by_receiver_id ~source_id id streams with
                   | None -> None
                   | Some s -> Some (s.id, x)) data) event streams
    |> E.fmap list_to_option

  let get_measurements (api : Protocol.api) (ids : Stream.ID.t list)
      _user _body _env _state =
    api.kv#get
    >>= fun { source; _ } ->
    map_event source api.notifs.streams api.notifs.measures
    |> filter_if_needed ids
    |> E.map (to_json Measure.to_yojson)
    |> fun event -> Lwt.return (`Ev event)

  let get_parameters (api : Protocol.api) (ids : Stream.ID.t list)
      _user _body _env _state =
    api.kv#get
    >>= fun { source; _ } ->
    map_event source api.notifs.streams api.notifs.params
    |> filter_if_needed ids
    |> E.map (to_json Params.to_yojson)
    |> fun event -> Lwt.return (`Ev event)

  let get_streams (api : Protocol.api) (ids : Stream.ID.t list)
      _user _body _env _state =
    let event = match ids with
      | [] ->
        api.notifs.streams
        |> S.changes
        |> E.map Util_json.(List.to_yojson Stream.to_yojson)
      | ids ->
        api.notifs.streams
        |> S.changes
        |> E.fmap (list_to_option % List.filter (fun (s : Stream.t) -> mem s.id ids))
        |> E.map Util_json.(List.to_yojson Stream.to_yojson) in
    Lwt.return (`Ev event)
end

let to_json f (v : int * 'a ts) =
  Util_json.(Pair.to_yojson Int.to_yojson (ts_to_yojson f)) v

let get_measurements (api : Protocol.api) (id : Stream.ID.t) _user _body _env state =
  match find_receiver_by_stream_id id api.notifs.streams with
  | None -> Lwt.return @@ `Error "Stream not found"
  | Some id ->
    api.channel (Request.Get_measure id)
    >>= function
    | Ok x -> Lwt.return @@ `Value (to_json Measure.to_yojson x)
    | Error e -> Lwt.return @@ `Error (Request.error_to_string e)

let get_parameters (api : Protocol.api) (id : Stream.ID.t) _user _body _env _state =
  match find_receiver_by_stream_id id api.notifs.streams with
  | None -> Lwt.return @@ `Error "Stream not found"
  | Some id ->
    api.channel (Request.Get_params id)
    >>= function
    | Ok x -> Lwt.return @@ `Value (to_json Params.to_yojson x)
    | Error e -> Lwt.return @@ `Error (Request.error_to_string e)

let get_stream (api : Protocol.api) (id : Stream.ID.t) _user _body _env _state =
  let stream =
    List.find_opt (fun (s : Stream.t) -> Stream.ID.equal s.id id)
    @@ React.S.value api.notifs.streams in
  Lwt.return @@ `Value Util_json.(Option.to_yojson Stream.to_yojson stream)

let get_streams (api : Protocol.api) (ids : Stream.ID.t list) _user _body _env _state =
  let streams = React.S.value api.notifs.streams in
  let streams = match ids with
    | [] -> streams
    | ids -> List.filter (fun (s : Stream.t) ->
        match List.find_opt (Stream.ID.equal s.id) ids with
        | None -> false
        | Some _ -> true) streams in
  Lwt.return @@ `Value Util_json.(List.to_yojson Stream.to_yojson streams)
