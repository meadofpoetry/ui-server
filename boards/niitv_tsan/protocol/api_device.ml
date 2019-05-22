open Application_types
open Board_niitv_tsan_types
open Api_util

module Event = struct
  open Util_react

  let get_state (api : Protocol.api) _user _body _env _state =
    let event =
      S.changes api.notifs.state
      |> E.map Topology.state_to_yojson in
    Lwt.return (`Ev event)

  let get_input (api : Protocol.api) _user _body _env _state =
    let event =
      api.notifs.status
      |> E.map (fun (x : Parser.Status.t) -> input_to_yojson x.input)
      |> E.changes ~eq:(=) in
    Lwt.return (`Ev event)

  let get_status (api : Protocol.api) _user _body _env _state =
    let event = E.map (fun (x : Parser.Status.t) ->
        status_to_yojson x.basic) api.notifs.status in
    Lwt.return (`Ev event)

  let get_info (api : Protocol.api) _user _body _env _state =
    let event =
      S.changes api.notifs.devinfo
      |> E.map (Util_json.Option.to_yojson devinfo_to_yojson) in
    Lwt.return (`Ev event)

  let get_errors (api : Protocol.api) _user _body _env _state =
    let event = E.map (Util_json.List.to_yojson Deverr.to_yojson)
        api.notifs.deverr in
    Lwt.return (`Ev event)

  let get_t2mi_mode (api : Protocol.api) _user _body _env _state =
    let event = E.map (fun (x : config) ->
        t2mi_mode_to_yojson x.t2mi_mode)
      @@ S.changes api.kv#s in
    Lwt.return (`Ev event)

end

let reset (api : Protocol.api) _user _body _env _state =
  api.channel Request.Reset
  >>=? fun () -> Lwt.return `Unit

let set_t2mi_mode (api : Protocol.api) _user body _env _state =
  match t2mi_mode_of_yojson body with
  | Error e -> Lwt.return (`Error e)
  | Ok t2mi_mode ->
    api.kv#get
    >>= fun ({ input; _ } as config) ->
    api.channel Request.(Set_mode { input; t2mi_mode })
    >>=? fun () -> api.kv#set { config with t2mi_mode }
    >>= fun () -> Lwt.return `Unit

let set_input (api : Protocol.api) _user body _env _state =
  match input_of_yojson body with
  | Error e -> Lwt.return (`Error e)
  | Ok input ->
    api.kv#get
    >>= fun ({ t2mi_mode; _ } as config) ->
    api.channel Request.(Set_mode { input; t2mi_mode })
    >>=? fun () -> api.kv#set { config with input }
    >>= fun () -> Lwt.return `Unit

let set_port (api : Protocol.api) port _user body _env _state =
  match Util_json.Bool.of_yojson body with
  | Error e -> Lwt.return (`Error e)
  | Ok en ->
    let input = match input_of_enum port, en with
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
      >>=? fun () -> api.kv#set { config with input }
      >>= fun () -> Lwt.return `Unit

let set_jitter_mode (api : Protocol.api) _user body _env _state =
  match jitter_mode_of_yojson body with
  | Error e -> Lwt.return (`Error e)
  | Ok jitter_mode ->
    api.channel Request.(Set_jitter_mode jitter_mode)
    >>=? fun () -> api.kv#get
    >>= fun config -> api.kv#set { config with jitter_mode }
    >>= fun () -> Lwt.return `Unit

let get_state (api : Protocol.api) _user _body _env _state =
  return_value
  @@ Topology.state_to_yojson
  @@ React.S.value api.notifs.state

let get_info (api : Protocol.api) force _user _body _env _state =
  match force with
  | None | Some false ->
    (match React.S.value api.notifs.devinfo with
     | None -> return_error Request.Not_responding
     | Some x -> return_value (devinfo_to_yojson x))
  | Some true ->
    api.channel Request.Get_devinfo
    >>=? return_value % devinfo_to_yojson

let get_errors (api : Protocol.api) timeout force _user _body _env _state =
  let timeout = match timeout with
    | None -> None
    | Some x -> Some (int_ms_to_float_s x) in
  match force with
  | None | Some false ->
    let timeout = match timeout with
      | None -> Fsm.status_timeout
      | Some x -> x in
    Lwt.pick
      [ (Boards.Board.await_no_response api.notifs.state >>= not_responding)
      ; (Util_react.E.next api.notifs.deverr >>= Lwt.return_ok)
      ; (Lwt_unix.sleep timeout >>= fun () -> Lwt.return_ok []) ]
    >>=? return_value % Util_json.List.to_yojson Deverr.to_yojson
  | Some true ->
    let request_id = Request_id.next () in
    api.channel (Request.Get_deverr { request_id; timeout })
    >>=? return_value % Util_json.List.to_yojson Deverr.to_yojson

let get_status (api : Protocol.api) _user _body _env _state =
  Lwt.pick
    [ (Boards.Board.await_no_response api.notifs.state >>= not_responding)
    ; (Util_react.E.next api.notifs.status >>= Lwt.return_ok)
    ; Fsm.sleep Fsm.status_timeout ]
  >>=? fun status -> return_value (status_to_yojson status.basic)

let get_input (api : Protocol.api) _user _body _env _state =
  Lwt.pick
    [ (Boards.Board.await_no_response api.notifs.state >>= not_responding)
    ; (Util_react.E.next api.notifs.status >>= Lwt.return_ok)
    ; Fsm.sleep Fsm.status_timeout ]
  >>=? fun status -> return_value (input_to_yojson status.input)

let get_t2mi_mode (api : Protocol.api) force _user _body _env _state =
  (match force with
   | None | Some false ->
     api.kv#get >>= fun { t2mi_mode; _ } -> Lwt.return_ok t2mi_mode
   | Some true ->
     Lwt.pick
       [ (Boards.Board.await_no_response api.notifs.state >>= not_responding)
       ; (Util_react.E.next api.notifs.status
          >>= fun s -> Lwt.return_ok s.t2mi_mode)
       ; Fsm.sleep Fsm.status_timeout ])
  >>=? return_value % t2mi_mode_to_yojson

let get_jitter_mode (api : Protocol.api) force _user _body _env _state =
  (match force with
   | None | Some false ->
     api.kv#get >>= fun { jitter_mode; _ } -> Lwt.return_ok jitter_mode
   | Some true ->
     Lwt.pick
       [ (Boards.Board.await_no_response api.notifs.state >>= not_responding)
       ; (Util_react.E.next api.notifs.status
          >>= fun s -> Lwt.return_ok s.jitter_mode)
       ; Fsm.sleep Fsm.status_timeout ])
  >>=? return_value % jitter_mode_to_yojson
