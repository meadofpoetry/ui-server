open Application_types
open Board_niitv_tsan_types
open Api_util

let ( >>= ) = Lwt.( >>= )

let get_streams (api : Protocol.api) _user _body _env _state =
  let streams = React.S.value api.notifs.streams in
  let to_json = Util_json.List.to_yojson Stream.to_yojson in
  Lwt.return (`Value (to_json streams))

let get_stream (api : Protocol.api) id _user _body _env _state =
  let streams = React.S.value api.notifs.streams in
  let to_json = Util_json.Option.to_yojson Stream.to_yojson in
  let stream = to_json @@ Stream.find_by_id id streams in
  Lwt.return (`Value stream)

let get_bitrate (api : Protocol.api) id _user _body _env _state =
  Lwt.pick
    [ Protocol.await_no_response api.notifs.state
    ; (Util_react.E.next api.notifs.bitrate >>= Lwt.return_ok)
    ; Fsm.sleep Fsm.status_timeout ]
  >>= function
  | Error e -> Lwt.return (`Error (Request.error_to_string e))
  | Ok bitrate ->
    return_value
    @@ Util_json.Option.to_yojson Bitrate.to_yojson
    @@ find_by_id id bitrate

let get_ts_info (api : Protocol.api) id _user _body _env _state =
  check_state api.notifs.state (fun () ->
      return_value
      @@ Util_json.Option.to_yojson TS_info.to_yojson
      @@ find_map_by_id id (fun (s : Structure.t) -> s.info)
      @@ React.S.value api.notifs.structure)

let get_pids (api : Protocol.api) id _user _body _env _state =
  check_state api.notifs.state (fun () ->
      return_value
      @@ Util_json.Option.to_yojson pids_to_yojson
      @@ find_map_by_id id (fun (s : Structure.t) -> s.pids)
      @@ React.S.value api.notifs.structure)

let get_si_psi_tables (api : Protocol.api) id _user _body _env _state =
  check_state api.notifs.state (fun () ->
      return_value
      @@ Util_json.Option.to_yojson si_psi_tables_to_yojson
      @@ find_map_by_id id (fun (s : Structure.t) -> s.tables)
      @@ React.S.value api.notifs.structure)

let get_services (api : Protocol.api) id _user _body _env _state =
  check_state api.notifs.state (fun () ->
      return_value
      @@ Util_json.Option.to_yojson services_to_yojson
      @@ find_map_by_id id (fun (s : Structure.t) -> s.services)
      @@ React.S.value api.notifs.structure)

let get_t2mi_info (api : Protocol.api) id _user _body _env _state =
  check_state api.notifs.state (fun () ->
      return_value
      @@ Util_json.Option.to_yojson t2mi_info_to_yojson
      @@ find_by_id id
      @@ React.S.value api.notifs.t2mi_info)

let get_section (api : Protocol.api) stream_id table_id
    section table_id_ext id_ext_1 id_ext_2
    _user _body _env _state =
  let streams = React.S.value api.notifs.streams in
  (match Stream.find_by_id stream_id streams with
   | Some { orig_id = TS_multi stream_id; _ } ->
     let request_id = Serializer.get_request_id () in
     let req = Request.Get_section { request_id
                                   ; stream_id
                                   ; table_id
                                   ; table_id_ext
                                   ; id_ext_1
                                   ; id_ext_2
                                   ; section
                                   } in
     api.channel req
   | Some _ -> Lwt.return_error Request.(Custom "Unsupported stream format")
   | None -> Lwt.return_error Request.(Custom "Stream not found"))
  >>= function
  | Error e -> Lwt.return (`Error (Request.error_to_string e))
  | Ok x ->
    let to_json = ts_to_yojson SI_PSI_section.Dump.to_yojson in
    Lwt.return (`Value (to_json x))
