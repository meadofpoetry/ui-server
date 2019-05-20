open Board_niitv_ts2ip_types
open Application_types
open Api_util
open Netlib

module Event = struct
  open Util_react

  let get_state (api : Protocol.api) _user _body _env _state =
    let event =
      S.changes api.notifs.state
      |> E.map Application_types.Topology.state_to_yojson in
    Lwt.return (`Ev event)

  let get_config (api : Protocol.api) _user _body _env _state =
    let event =
      S.changes api.notifs.config
      |> E.map config_to_yojson in
    Lwt.return (`Ev event)

  let get_mode (api : Protocol.api) _user _body _env _state =
    let event =
      E.map (fun (x : config) -> mode_to_yojson x.mode)
      @@ React.S.changes api.notifs.config in
    Lwt.return (`Ev event)

  let get_network_mode (api : Protocol.api) _user _body _env _state =
    let event =
      E.map (fun (x : config) -> network_mode_to_yojson x.mode.network)
      @@ React.S.changes api.notifs.config in
    Lwt.return (`Ev event)

  let get_status (api : Protocol.api) _user _body _env _state =
    let event = E.map device_status_to_yojson api.notifs.device_status in
    Lwt.return (`Ev event)

end

let get_state (api : Protocol.api) _user _body _env _state =
  let value = React.S.value api.notifs.state in
  return_value (Application_types.Topology.state_to_yojson value)

let get_status (api : Protocol.api) _user _body _env _state =
  Lwt.pick
    [ (Boards.Board.await_no_response api.notifs.state >>= not_responding)
    ; (Util_react.E.next api.notifs.device_status >>= Lwt.return_ok)
    ; Fsm.sleep Fsm.status_timeout ]
  >>=? return_value % device_status_to_yojson

let get_info (api : Protocol.api) force _user _body _env _state =
  match force with
  | None | Some false ->
    (match React.S.value api.notifs.devinfo with
     | None -> return_error Request.Not_responding
     | Some x -> return_value @@ devinfo_to_yojson x)
  | Some true ->
    api.channel Request.Get_devinfo
    >>=? return_value % devinfo_to_yojson

let get_config (api : Protocol.api) _user _body _env _state =
  api.kv#get >>= return_value % config_to_yojson

let get_mode (api : Protocol.api) _user _body _env _state =
  api.kv#get >>= fun cfg -> return_value (mode_to_yojson cfg.mode)

let get_mac (api : Protocol.api) _user _body _env _state =
  api.kv#get >>= fun cfg -> return_value (Macaddr.to_yojson cfg.mac)

let get_network (api : Protocol.api) _user _body _env _state =
  api.kv#get
  >>= fun cfg -> return_value (network_mode_to_yojson cfg.mode.network)

let get_ip_address (api : Protocol.api) _user _body _env _state =
  api.kv#get
  >>= fun cfg -> return_value (Ipaddr.V4.to_yojson cfg.mode.network.ip)

let get_mask (api : Protocol.api) _user _body _env _state =
  api.kv#get
  >>= fun cfg -> return_value (Ipaddr.V4.to_yojson cfg.mode.network.mask)

let get_gateway (api : Protocol.api) _user _body _env _state =
  api.kv#get
  >>= fun cfg -> return_value (Ipaddr.V4.to_yojson cfg.mode.network.gateway)

let set_mac (api : Protocol.api) force _user body _env _state =
  match Macaddr.of_yojson body with
  | Error e -> Lwt.return (`Error e)
  | Ok mac ->
    api.kv#get
    >>= fun config ->
    (match force with
     | None | Some false -> Lwt.return_ok ()
     | Some true -> api.channel Request.(Set_mac mac))
    >>=? fun () -> api.kv#set { config with mac }
    >>= fun () -> Lwt.return `Unit

let set_nw (api : Protocol.api) body _of f =
  match _of body with
  | Error e -> Lwt.return (`Error e)
  | Ok v ->
    api.kv#get
    >>= fun config ->
    let mode = f config v in
    let main, _, _ = Request.split_mode mode in
    Lwt_result.Infix.(
      api.channel (Request.Set_mode_main main)
      >>= fun () -> Lwt.return_ok mode)
    >>=? fun mode -> api.kv#set { config with mode }
    >>= fun () -> Lwt.return `Unit

let set_mode (api : Protocol.api) _user body _env _state =
  match mode_of_yojson body with
  | Error e -> Lwt.return (`Error e)
  | Ok mode ->
    let devinfo = React.S.value api.notifs.devinfo in
    match Streams_setup.full devinfo mode.udp with
    | Error e -> Lwt.return (`Error (Stream.Table.set_error_to_string e))
    | Ok udp ->
      let main, aux_1, aux_2 = Request.split_mode { mode with udp } in
      Lwt_result.Infix.(
        api.channel (Request.Set_mode_main main)
        >>= fun () -> api.channel (Request.Set_mode_aux_1 aux_1)
        >>= fun () -> api.channel (Request.Set_mode_aux_2 aux_2)
        >>= fun () -> Lwt.return_ok mode)
      >>=? fun mode -> api.kv#get
      >>= fun config -> api.kv#set { config with mode }
      >>= fun () -> Lwt.return `Unit

let set_network (api : Protocol.api) _user body _env _state =
  set_nw api body network_mode_of_yojson (fun cfg network ->
      { cfg.mode with network })

let set_ip_address (api : Protocol.api) _user body _env _state =
  set_nw api body Ipaddr.V4.of_yojson (fun cfg ip ->
      let network = { cfg.mode.network with ip } in
      { cfg.mode with network })

let set_mask (api : Protocol.api) _user body _env _state =
  set_nw api body Ipaddr.V4.of_yojson (fun cfg mask ->
      let network = { cfg.mode.network with mask } in
      { cfg.mode with network })

let set_gateway (api : Protocol.api) _user body _env _state =
  set_nw api body Ipaddr.V4.of_yojson (fun cfg gateway ->
      let network = { cfg.mode.network with gateway } in
      { cfg.mode with network })
