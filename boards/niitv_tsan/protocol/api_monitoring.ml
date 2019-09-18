open Application_types
open Board_niitv_tsan_types
open Api_util

module Event = struct
  open Util_react

  let filter_ids ids v =
    match filter_ids ids v with
    | [] -> None
    | x -> Some x

  let get_bitrate (api : Protocol.api) ids _user =
    let to_yojson = stream_assoc_list_to_yojson Bitrate.cur_to_yojson in
    let event = E.fmap (Option.map to_yojson % filter_ids ids) api.notifs.bitrate in
    Lwt.return event

  let get_bitrate_with_stats (api : Protocol.api) ids _user =
    let to_yojson = stream_assoc_list_to_yojson Bitrate.ext_to_yojson in
    let event = E.fmap (Option.map to_yojson % filter_ids ids) api.notifs.bitrate_ext in
    Lwt.return event

  let get_ts_info (api : Protocol.api) ids _user =
    let to_yojson = stream_assoc_list_to_yojson TS_info.to_yojson in
    let get_info = List.map (fun (id, (x : Structure.t)) -> id, x.info) in
    let event =
      api.notifs.structure
      |> S.changes
      |> E.fmap (Option.map to_yojson % filter_ids ids % get_info)
    in
    Lwt.return event

  let get_pids (api : Protocol.api) ids _user =
    let to_yojson = stream_assoc_list_to_yojson pids_ts_to_yojson in
    let get_pids = List.map (fun (id, x) -> id, Structure.pids_ts x) in
    let event =
      api.notifs.structure
      |> S.changes
      |> E.fmap (Option.map to_yojson % filter_ids ids % get_pids)
    in
    Lwt.return event

  let get_si_psi_tables (api : Protocol.api) ids _user =
    let to_yojson = stream_assoc_list_to_yojson si_psi_tables_ts_to_yojson in
    let get_si_psi_tables = List.map (fun (id, x) -> id, Structure.tables_ts x) in
    let event =
      api.notifs.structure
      |> S.changes
      |> E.fmap (Option.map to_yojson % filter_ids ids % get_si_psi_tables)
    in
    Lwt.return event

  let get_services (api : Protocol.api) ids _user =
    let to_yojson = stream_assoc_list_to_yojson services_ts_to_yojson in
    let get_services = List.map (fun (id, x) -> id, Structure.services_ts x) in
    let event =
      api.notifs.structure
      |> S.changes
      |> E.fmap (Option.map to_yojson % filter_ids ids % get_services)
    in
    Lwt.return event
end

let filter_errors f x =
  match
    List.filter_map
      (fun (id, errors) ->
        match List.filter f errors with
        | [] -> None
        | e -> Some (id, e))
      x
  with
  | [] -> None
  | x -> Some x

let get_errors (api : Protocol.api) ids timeout _user _body _env _state =
  let timeout =
    match timeout with
    | None -> Fsm.status_timeout
    | Some x -> int_ms_to_float_s x
  in
  Lwt.pick
    [ Boards.Board.await_no_response api.notifs.state >>= not_responding
    ; Util_react.E.next api.notifs.errors >>= Lwt.return_ok
    ; (Lwt_unix.sleep timeout >>= fun () -> Lwt.return_ok []) ]
  >>=? fun errors ->
  return_value
  @@ stream_assoc_list_to_yojson (Util_json.List.to_yojson Error.to_yojson)
  @@ filter_ids ids errors

let get_bitrate (api : Protocol.api) ids timeout _user _body _env _state =
  let timeout =
    match timeout with
    | None -> Fsm.status_timeout
    | Some x -> int_ms_to_float_s x
  in
  Lwt.pick
    [ Boards.Board.await_no_response api.notifs.state >>= not_responding
    ; Util_react.E.next api.notifs.bitrate >>= Lwt.return_ok
    ; (Lwt_unix.sleep timeout >>= fun () -> Lwt.return_ok []) ]
  >>=? fun bitrate ->
  return_value
  @@ stream_assoc_list_to_yojson Bitrate.cur_to_yojson
  @@ filter_ids ids bitrate

let get_bitrate_with_stats (api : Protocol.api) ids timeout _user _body _env _state =
  let timeout =
    match timeout with
    | None -> Fsm.status_timeout
    | Some x -> int_ms_to_float_s x
  in
  Lwt.pick
    [ Boards.Board.await_no_response api.notifs.state >>= not_responding
    ; Util_react.E.next api.notifs.bitrate_ext >>= Lwt.return_ok
    ; (Lwt_unix.sleep timeout >>= fun () -> Lwt.return_ok []) ]
  >>=? fun bitrate ->
  return_value
  @@ stream_assoc_list_to_yojson Bitrate.ext_to_yojson
  @@ filter_ids ids bitrate

let reset_bitrate_stats (api : Protocol.api) ids _user _body _env _state =
  print_endline "resetting bitrate stats";
  (match ids with
  | [] -> Bitrate_queue.clear api.bitrate_queue
  | ids -> List.iter (Bitrate_queue.clear_stream api.bitrate_queue) ids);
  Lwt.return `Unit

let ( >>= ) = Lwt_result.bind

let get_ts_info (api : Protocol.api) force ids _user _body _env _state =
  (match force with
  | Some true ->
      let request_id = Request_id.next () in
      api.channel (Get_structure {request_id; stream = `All})
      >>= Lwt.return_ok % map_stream_id (React.S.value api.notifs.streams)
  | None | Some false -> Lwt.return_ok @@ React.S.value api.notifs.structure)
  >>=? return_value
       % stream_assoc_list_to_yojson TS_info.to_yojson
       % filter_ids ids
       % List.map (fun (id, (x : Structure.t)) -> id, x.info)

let get_pids (api : Protocol.api) force ids _user _body _env _state =
  (match force with
  | Some true ->
      let request_id = Request_id.next () in
      api.channel (Get_structure {request_id; stream = `All})
      >>= Lwt.return_ok % map_stream_id (React.S.value api.notifs.streams)
  | None | Some false -> Lwt.return_ok @@ React.S.value api.notifs.structure)
  >>=? return_value
       % stream_assoc_list_to_yojson pids_ts_to_yojson
       % filter_ids ids
       % List.map (fun (id, x) -> id, Structure.pids_ts x)

let get_si_psi_tables (api : Protocol.api) force ids _user _body _env _state =
  (match force with
  | Some true ->
      let request_id = Request_id.next () in
      api.channel (Get_structure {request_id; stream = `All})
      >>= Lwt.return_ok % map_stream_id (React.S.value api.notifs.streams)
  | None | Some false -> Lwt.return_ok @@ React.S.value api.notifs.structure)
  >>=? return_value
       % stream_assoc_list_to_yojson si_psi_tables_ts_to_yojson
       % filter_ids ids
       % List.map (fun (id, (s : Structure.t)) -> id, Structure.tables_ts s)

let get_services (api : Protocol.api) force ids _user _body _env _state =
  (match force with
  | Some true ->
      let request_id = Request_id.next () in
      api.channel (Get_structure {request_id; stream = `All})
      >>= Lwt.return_ok % map_stream_id (React.S.value api.notifs.streams)
  | None | Some false -> Lwt.return_ok @@ React.S.value api.notifs.structure)
  >>=? return_value
       % stream_assoc_list_to_yojson services_ts_to_yojson
       % filter_ids ids
       % List.map (fun (id, (s : Structure.t)) -> id, Structure.services_ts s)

let filter_t2mi_stream_id ids l =
  match ids with
  | [] -> l
  | ids ->
      List.filter_map
        (fun (id, l) ->
          match List.filter (fun (id, _) -> List.mem id ids) l with
          | [] -> None
          | l -> Some (id, l))
        l

let range i j =
  let rec up i j acc = if i = j then i :: acc else up i (pred j) (j :: acc)
  and down i j acc = if i = j then i :: acc else down i (succ j) (j :: acc) in
  if i <= j then up i j [] else down i j []

let get_t2mi_info (api : Protocol.api) force ids t2mi_stream_ids _user _body _env _state
    =
  (match force with
  | Some true -> (
      let rec loop acc = function
        | [] -> Lwt.return_ok @@ acc
        | t2mi_stream_id :: tl -> (
            let request_id = Request_id.next () in
            api.channel (Get_t2mi_info {request_id; t2mi_stream_id})
            >>= fun x ->
            let id =
              Boards.Util.List.find_map (fun (s : Stream.t) ->
                  match s.typ with
                  | T2MI -> Some s.id
                  | _ -> None)
              @@ React.S.value api.notifs.streams
            in
            match id with
            | None -> loop acc tl
            | Some id ->
                let acc =
                  Boards.Util.List.Assoc.update
                    ~eq:Stream.ID.equal
                    (function
                      | None -> Some [x]
                      | Some l -> Some (x :: l))
                    id
                    acc
                in
                loop acc tl)
      in
      match t2mi_stream_ids with
      | [] -> loop [] (range 0 7)
      | l -> loop [] l)
  | None | Some false ->
      check_state api.notifs.state
      >>= fun () -> Lwt.return_ok @@ React.S.value api.notifs.t2mi_info)
  >>=? return_value
       % stream_assoc_list_to_yojson t2mi_info_to_yojson
       % filter_t2mi_stream_id t2mi_stream_ids
       % filter_ids ids
