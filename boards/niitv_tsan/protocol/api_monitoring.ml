open Application_types
open Board_niitv_tsan_types
open Api_util

module List = Boards.Util.List

let filter_errors f x =
  match List.filter_map (fun (id, errors) ->
      match List.filter f errors with
      | [] -> None
      | e -> Some (id, e)) x with
  | [] -> None
  | x -> Some x

let get_errors (api : Protocol.api) ids timeout _user _body _env _state =
  let timeout = match timeout with
    | None -> Fsm.status_timeout
    | Some x -> x in
  Lwt.pick
    [ Protocol.await_no_response api.notifs.state
    ; (Util_react.E.next api.notifs.errors >>= Lwt.return_ok)
    ; (Lwt_unix.sleep timeout >>= fun () -> Lwt.return_ok []) ]
  >>=? fun errors ->
  return_value
  @@ stream_assoc_list_to_yojson (Util_json.List.to_yojson Error.to_yojson)
  @@ filter_ids ids errors

let get_filtered_errors ~is_t2mi (api : Protocol.api) ids timeout
    _user _body _env _state =
  let timeout = match timeout with
    | None -> Fsm.status_timeout
    | Some x -> x in
  let event = React.E.fmap (filter_errors (fun (x : 'a Error.e) ->
      is_t2mi = x.is_t2mi)) api.notifs.errors in
  let waiter =
    Lwt.pick
      [ Protocol.await_no_response api.notifs.state
      ; (Util_react.E.next event >>= Lwt.return_ok)
      ; (Lwt_unix.sleep timeout >>= fun () -> Lwt.return_ok []) ] in
  Lwt.on_termination waiter (fun () -> React.E.stop event);
  waiter >>=? fun errors ->
  return_value
  @@ stream_assoc_list_to_yojson (Util_json.List.to_yojson Error.to_yojson)
  @@ filter_ids ids errors

let get_bitrate (api : Protocol.api) ids timeout _user _body _env _state =
  let timeout = match timeout with
    | None -> Fsm.status_timeout
    | Some x -> x in
  Lwt.pick
    [ Protocol.await_no_response api.notifs.state
    ; (Util_react.E.next api.notifs.bitrate >>= Lwt.return_ok)
    ; (Lwt_unix.sleep timeout >>= fun () -> Lwt.return_ok []) ]
  >>=? fun bitrate ->
  return_value
  @@ stream_assoc_list_to_yojson Bitrate.to_yojson
  @@ filter_ids ids bitrate

let get_ts_info (api : Protocol.api) ids _user _body _env _state =
  check_state api.notifs.state (fun () ->
      return_value
      @@ stream_assoc_list_to_yojson TS_info.to_yojson
      @@ filter_ids ids
      @@ List.map (fun (id, (s : Structure.t)) -> id, s.info)
      @@ React.S.value api.notifs.structure)

let get_pids (api : Protocol.api) ids _user _body _env _state =
  check_state api.notifs.state (fun () ->
      return_value
      @@ stream_assoc_list_to_yojson pids_to_yojson
      @@ filter_ids ids
      @@ List.map (fun (id, (s : Structure.t)) -> id, s.pids)
      @@ React.S.value api.notifs.structure)

let get_si_psi_tables (api : Protocol.api) ids _user _body _env _state =
  check_state api.notifs.state (fun () ->
      return_value
      @@ stream_assoc_list_to_yojson si_psi_tables_to_yojson
      @@ filter_ids ids
      @@ List.map (fun (id, (s : Structure.t)) -> id, s.tables)
      @@ React.S.value api.notifs.structure)

let get_services (api : Protocol.api) ids _user _body _env _state =
  check_state api.notifs.state (fun () ->
      return_value
      @@ stream_assoc_list_to_yojson services_to_yojson
      @@ filter_ids ids
      @@ List.map (fun (id, (s : Structure.t)) -> id, s.services)
      @@ React.S.value api.notifs.structure)

let get_t2mi_info (api : Protocol.api) ids _user _body _env _state =
  check_state api.notifs.state (fun () ->
      return_value
      @@ stream_assoc_list_to_yojson t2mi_info_to_yojson
      @@ filter_ids ids
      @@ React.S.value api.notifs.t2mi_info)
