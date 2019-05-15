open Application_types
open Board_niitv_tsan_types
open Api_util

let ( >>= ) = Lwt.( >>= )

let get_bitrate (api : Protocol.api) ids _user _body _env _state =
  Lwt.pick
    [ Protocol.await_no_response api.notifs.state
    ; (Util_react.E.next api.notifs.bitrate >>= Lwt.return_ok)
    ; Fsm.sleep Fsm.status_timeout ]
  >>= function
  | Error e -> Lwt.return (`Error (Request.error_to_string e))
  | Ok bitrate ->
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
