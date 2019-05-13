open Board_niitv_ts2ip_types
open Application_types

let ( >>= ) = Lwt.( >>= )

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
  Lwt.return (`Value (Application_types.Topology.state_to_yojson value))

let get_status (api : Protocol.api) _user _body _env _state =
  match React.S.value api.notifs.state with
  | `No_response | `Init ->
    let error = Request.error_to_string Not_responding in
    Lwt.return (`Error error)
  | `Fine ->
    Lwt.pick
      [ Protocol.await_no_response api.notifs.state
      ; (Util_react.E.next api.notifs.device_status >>= Lwt.return_ok)
      ]
    >>= function
    | Error e -> Lwt.return (`Error (Request.error_to_string e))
    | Ok x -> Lwt.return (`Value (device_status_to_yojson x))

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

let get_config (api : Protocol.api) _user _body _env _state =
  api.kv#get
  >>= fun config -> Lwt.return (`Value (config_to_yojson config))

let get_mode (api : Protocol.api) _user _body _env _state =
  api.kv#get
  >>= fun config -> Lwt.return (`Value (mode_to_yojson config.mode))

let get_mac (api : Protocol.api) _user _body _env _state =
  api.kv#get
  >>= fun config -> Lwt.return (`Value (Netlib.Macaddr.to_yojson config.mac))

let get_network (api : Protocol.api) _user _body _env _state =
  api.kv#get
  >>= fun config ->
  Lwt.return (`Value (network_mode_to_yojson config.mode.network))

let get_ip_address (api : Protocol.api) _user _body _env _state =
  api.kv#get
  >>= fun config ->
  Lwt.return (`Value (Netlib.Ipaddr.V4.to_yojson config.mode.network.ip))

let get_mask (api : Protocol.api) _user _body _env _state =
  api.kv#get
  >>= fun config ->
  Lwt.return (`Value (Netlib.Ipaddr.V4.to_yojson config.mode.network.mask))

let get_gateway (api : Protocol.api) _user _body _env _state =
  api.kv#get
  >>= fun config ->
  Lwt.return (`Value (Netlib.Ipaddr.V4.to_yojson config.mode.network.gateway))

let set_mac (api : Protocol.api) force _user body _env _state =
  match Netlib.Macaddr.of_yojson body with
  | Error e -> Lwt.return (`Error e)
  | Ok mac ->
    api.kv#get
    >>= fun config ->
    (match force with
     | None | Some false -> Lwt.return_ok ()
     | Some true -> api.channel Request.(Set_mac mac))
    >>= function
    | Error e -> Lwt.return (`Error (Request.error_to_string e))
    | Ok () ->
      api.kv#set { config with mac }
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
    >>= function
    | Error e -> Lwt.return (`Error Request.(error_to_string e))
    | Ok mode ->
      api.kv#set { config with mode }
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
      >>= function
      | Error e -> Lwt.return (`Error (Request.error_to_string e))
      | Ok mode ->
        api.kv#get
        >>= fun config ->
        api.kv#set { config with mode }
        >>= fun () -> Lwt.return `Unit

let set_network (api : Protocol.api) _user body _env _state =
  set_nw api body network_mode_of_yojson (fun cfg network ->
      { cfg.mode with network })

let set_ip_address (api : Protocol.api) _user body _env _state =
  set_nw api body Netlib.Ipaddr.V4.of_yojson (fun cfg ip ->
      let network = { cfg.mode.network with ip } in
      { cfg.mode with network })

let set_mask (api : Protocol.api) _user body _env _state =
  set_nw api body Netlib.Ipaddr.V4.of_yojson (fun cfg mask ->
      let network = { cfg.mode.network with mask } in
      { cfg.mode with network })

let set_gateway (api : Protocol.api) _user body _env _state =
  set_nw api body Netlib.Ipaddr.V4.of_yojson (fun cfg gateway ->
      let network = { cfg.mode.network with gateway } in
      { cfg.mode with network })
