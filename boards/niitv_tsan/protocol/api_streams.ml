open Application_types
open Board_niitv_tsan_types
open Api_util

let ( >>= ) = Lwt.( >>= )

let ( >>=? ) x f =
  x >>= function
  | Ok x -> f x
  | Error e -> return_error e

let invalid_stream = Request.Custom "Unsupported stream format"

let stream_not_found = Request.Custom "Stream not found"

let find_multi_id id streams =
  match Stream.find_by_id id @@ React.S.value streams with
  | None -> Error stream_not_found
  | Some s -> match s.orig_id with
    | TS_multi id -> Ok id
    | _ -> Error invalid_stream

let return_value_or_not_found = function
  | None -> return_error stream_not_found
  | Some x -> return_value x

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
  >>=? fun bitrate ->
  return_value
  @@ Util_json.Option.to_yojson Bitrate.to_yojson
  @@ find_by_id id bitrate

let get_ts_info (api : Protocol.api) id force _user _body _env _state =
  match force with
  | None | Some false ->
    check_state api.notifs.state (fun () ->
        return_value_or_not_found
        @@ find_map_by_id id (TS_info.to_yojson % Structure.info)
        @@ React.S.value api.notifs.structure)
  | Some true ->
    match find_multi_id id api.notifs.streams with
    | Error e -> return_error e
    | Ok id ->
      let request_id = Serializer.get_request_id () in
      let req = Request.Get_structure { request_id; stream = `Single id } in
      api.channel req
      >>=? fun structures ->
      match List.assoc_opt id structures with
      | None -> return_error stream_not_found
      | Some x -> return_value (TS_info.to_yojson x.info)

let get_pids (api : Protocol.api) id force _user _body _env _state =
  match force with
  | None | Some false ->
    check_state api.notifs.state (fun () ->
        return_value_or_not_found
        @@ find_map_by_id id (pids_to_yojson % Structure.pids)
        @@ React.S.value api.notifs.structure)
  | Some true ->
    match find_multi_id id api.notifs.streams with
    | Error e -> return_error e
    | Ok id ->
      let request_id = Serializer.get_request_id () in
      let req = Request.Get_structure { request_id; stream = `Single id } in
      api.channel req
      >>=? fun structures ->
      match List.assoc_opt id structures with
      | None -> return_error stream_not_found
      | Some x -> return_value (pids_to_yojson x.pids)

let get_si_psi_tables (api : Protocol.api) id _user _body _env _state =
  check_state api.notifs.state (fun () ->
      return_value_or_not_found
      @@ find_map_by_id id (si_psi_tables_to_yojson % Structure.tables)
      @@ React.S.value api.notifs.structure)

let get_services (api : Protocol.api) id _user _body _env _state =
  check_state api.notifs.state (fun () ->
      return_value_or_not_found
      @@ find_map_by_id id (services_to_yojson % Structure.services)
      @@ React.S.value api.notifs.structure)

let get_t2mi_info (api : Protocol.api) id _user _body _env _state =
  check_state api.notifs.state (fun () ->
      return_value_or_not_found
      @@ find_map_by_id id t2mi_info_to_yojson
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
   | Some _ -> Lwt.return_error invalid_stream
   | None -> Lwt.return_error stream_not_found)
  >>=? fun x -> return_value @@ ts_to_yojson SI_PSI_section.Dump.to_yojson x
