open Application_types
open Board_niitv_tsan_types

let ( >>= ) = Lwt.( >>= )

let reset (api : Protocol.api) _user _body _env _state =
  api.channel Request.Reset
  >>= function
  | Error e -> Lwt.return (`Error (Request.error_to_string e))
  | Ok () -> Lwt.return `Unit

let set_t2mi_mode (api : Protocol.api) _user body _env _state =
  match t2mi_mode_of_yojson body with
  | Error e -> Lwt.return (`Error e)
  | Ok t2mi_mode ->
    api.kv#get
    >>= fun ({ input; _ } as config) ->
    api.channel Request.(Set_mode { input; t2mi_mode })
    >>= function
    | Error e -> Lwt.return (`Error (Request.error_to_string e))
    | Ok () ->
      api.kv#set { config with t2mi_mode }
      >>= fun () -> Lwt.return `Unit

let set_input (api : Protocol.api) _user body _env _state =
  match input_of_yojson body with
  | Error e -> Lwt.return (`Error e)
  | Ok input ->
    api.kv#get
    >>= fun ({ t2mi_mode; _ } as config) ->
    api.channel Request.(Set_mode { input; t2mi_mode })
    >>= function
    | Error e -> Lwt.return (`Error (Request.error_to_string e))
    | Ok () ->
      api.kv#set { config with input }
      >>= fun () -> Lwt.return `Unit

let set_port (api : Protocol.api) port _user body _env _state =
  match Util_json.Bool.of_yojson body with
  | Error e -> Lwt.return (`Error e)
  | Ok en ->
    let input = match input_of_int port, en with
      | Some i, true -> Some i
      | Some ASI, false -> Some SPI
      | Some SPI, false -> Some ASI
      | _ -> None in
    match input with
    | None -> Lwt.return (`Error (Printf.sprintf "Unknown port %d" port))
    | Some input ->
      api.kv#get
      >>= fun ({ t2mi_mode; _ } as config) ->
      api.channel Request.(Set_mode { input; t2mi_mode })
      >>= function
      | Error e -> Lwt.return (`Error (Request.error_to_string e))
      | Ok () ->
        api.kv#set { config with input }
        >>= fun () -> Lwt.return `Unit

let set_jitter_mode (api : Protocol.api) _user body _env _state =
  match jitter_mode_of_yojson body with
  | Error e -> Lwt.return (`Error e)
  | Ok jitter_mode ->
    api.channel Request.(Set_jitter_mode jitter_mode)
    >>= function
    | Error e -> Lwt.return (`Error (Request.error_to_string e))
    | Ok () ->
      api.kv#get
      >>= fun config -> api.kv#set { config with jitter_mode }
      >>= fun () -> Lwt.return `Unit

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
    ; (Util_react.E.next api.notifs.status >>= Lwt.return_ok)
    ; Fsm.sleep Fsm.status_timeout ]
  >>= function
  | Error e -> Lwt.return (`Error (Request.error_to_string e))
  | Ok status -> Lwt.return (`Value (status_to_yojson status.basic))

let get_input (api : Protocol.api) _user _body _env _state =
  Lwt.pick
    [ Protocol.await_no_response api.notifs.state
    ; (Util_react.E.next api.notifs.status >>= Lwt.return_ok)
    ; Fsm.sleep Fsm.status_timeout ]
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
          >>= fun s -> Lwt.return_ok s.t2mi_mode)
       ; Fsm.sleep Fsm.status_timeout ])
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
          >>= fun s -> Lwt.return_ok s.jitter_mode)
       ; Fsm.sleep Fsm.status_timeout ])
  >>= function
  | Error e -> Lwt.return (`Error (Request.error_to_string e))
  | Ok x -> Lwt.return (`Value (jitter_mode_to_yojson x))
