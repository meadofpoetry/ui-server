open Application_types
open Board_niitv_tsan_types

let ( >>= ) = Lwt.( >>= )

let get_state (api : Protocol.api) _user _body _env _state =
  let json = Topology.state_to_yojson @@ React.S.value api.notifs.state in
  Lwt.return (`Value json)

let get_info (api : Protocol.api) force _user _body _env _state =
  match force with
  | None | Some false ->
    (match React.S.value api.notifs.devinfo with
     | None -> Lwt.return (`Error (Request.error_to_string Not_responding))
     | Some x -> Lwt.return (`Value (devinfo_to_yojson x)))
  | Some true ->
    api.channel Request.Get_devinfo
    >>= function
    | Ok x -> Lwt.return (`Value (devinfo_to_yojson x))
    | Error e -> Lwt.return (`Error (Request.error_to_string e))

let get_status (api : Protocol.api) _user _body _env _state =
  Lwt.pick
    [ Protocol.await_no_response api.notifs.state
    ; (Util_react.E.next api.notifs.status >>= Lwt.return_ok) ]
  >>= function
  | Error e -> Lwt.return (`Error (Request.error_to_string e))
  | Ok status -> Lwt.return (`Value (status_to_yojson status.basic))

let get_input (api : Protocol.api) _user _body _env _state =
  Lwt.pick
    [ Protocol.await_no_response api.notifs.state
    ; (Util_react.E.next api.notifs.status >>= Lwt.return_ok) ]
  >>= function
  | Error e -> Lwt.return (`Error (Request.error_to_string e))
  | Ok status -> Lwt.return (`Value (input_to_yojson status.input))

let get_t2mi_mode (api : Protocol.api) force _user _body _env _state =
  (match force with
   | None | Some false ->
     api.kv#get
     >>= fun { t2mi_mode; _ } -> Lwt.return_ok t2mi_mode
   | Some true ->
     Lwt.pick
       [ Protocol.await_no_response api.notifs.state
       ; (Util_react.E.next api.notifs.status
          >>= fun s -> Lwt.return_ok s.t2mi_mode) ])
  >>= function
  | Error e -> Lwt.return (`Error (Request.error_to_string e))
  | Ok x -> Lwt.return (`Value (t2mi_mode_to_yojson x))

let get_jitter_mode (api : Protocol.api) force _user _body _env _state =
  (match force with
   | None | Some false ->
     api.kv#get
     >>= fun { jitter_mode; _ } -> Lwt.return_ok jitter_mode
   | Some true ->
     Lwt.pick
       [ Protocol.await_no_response api.notifs.state
       ; (Util_react.E.next api.notifs.status
          >>= fun s -> Lwt.return_ok s.jitter_mode) ])
  >>= function
  | Error e -> Lwt.return (`Error (Request.error_to_string e))
  | Ok x ->
    let to_json = Util_json.Option.to_yojson jitter_mode_to_yojson in
    Lwt.return (`Value (to_json x))
