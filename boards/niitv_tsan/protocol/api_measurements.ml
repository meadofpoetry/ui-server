open Application_types
open Board_niitv_tsan_types

let ( >>= ) = Lwt.( >>= )

let to_yojson f = Util_json.(
    List.to_yojson @@ Pair.to_yojson Stream.ID.to_yojson f)

let get_bitrate (api : Protocol.api) _user _body _env _state =
  Lwt.pick
    [ Protocol.await_no_response api.notifs.state
    ; (Util_react.E.next api.notifs.bitrate >>= Lwt.return_ok)
    ; Fsm.sleep Fsm.status_timeout ]
  >>= function
  | Error e -> Lwt.return (`Error (Request.error_to_string e))
  | Ok bitrate -> Lwt.return (`Value (to_yojson Bitrate.to_yojson bitrate))

let return_value x =
  Lwt.return (`Value x)

let check_state state f =
  match React.S.value state with
  | `Fine -> f ()
  | `No_response | `Init ->
    Lwt.return @@ `Error (Request.error_to_string Request.Not_responding)

let get_ts_info (api : Protocol.api) _user _body _env _state =
  check_state api.notifs.state (fun () ->
      return_value
      @@ to_yojson TS_info.to_yojson
      @@ List.map (fun (id, (s : Structure.t)) -> id, s.info)
      @@ React.S.value api.notifs.structure)

let get_pids (api : Protocol.api) _user _body _env _state =
  check_state api.notifs.state (fun () ->
      let to_json = Util_json.(
          List.to_yojson
          @@ Pair.to_yojson Int.to_yojson PID_info.to_yojson) in
      return_value
      @@ to_yojson to_json
      @@ List.map (fun (id, (s : Structure.t)) -> id, s.pids)
      @@ React.S.value api.notifs.structure)

let get_si_psi_tables (api : Protocol.api) _user _body _env _state =
  check_state api.notifs.state (fun () ->
      let to_json = Util_json.(
          List.to_yojson
          @@ Pair.to_yojson
            SI_PSI_table.id_to_yojson
            SI_PSI_table.to_yojson) in
      return_value
      @@ to_yojson to_json
      @@ List.map (fun (id, (s : Structure.t)) -> id, s.tables)
      @@ React.S.value api.notifs.structure)

let get_services (api : Protocol.api) _user _body _env _state =
  check_state api.notifs.state (fun () ->
      let to_json = Util_json.(
          List.to_yojson
          @@ Pair.to_yojson Int.to_yojson Service_info.to_yojson) in
      return_value
      @@ to_yojson to_json
      @@ List.map (fun (id, (s : Structure.t)) -> id, s.services)
      @@ React.S.value api.notifs.structure)

let get_t2mi_info (api : Protocol.api) _user _body _env _state =
  check_state api.notifs.state (fun () ->
      let to_json = Util_json.(
          List.to_yojson
          @@ Pair.to_yojson Int.to_yojson T2mi_info.to_yojson) in
      return_value
      @@ to_yojson to_json
      @@ React.S.value api.notifs.t2mi_info)
