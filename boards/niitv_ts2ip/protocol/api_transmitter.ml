open Board_niitv_ts2ip_types
open Application_types

let ( >>= ) = Lwt.( >>= )

module Event = struct
  open Util_react

  let get_status (api : Protocol.api) _user _body _env _state =
    let event =
      E.map transmitter_status_to_yojson
        api.notifs.transmitter_status in
    Lwt.return (`Ev event)

  let get_incoming_streams (api : Protocol.api) _user _body _env _state =
    let event =
      E.map Util_json.(List.to_yojson Stream.to_yojson)
      @@ React.S.changes api.notifs.incoming_streams in
    Lwt.return (`Ev event)

  let get_outgoing_streams (api : Protocol.api) _user _body _env _state =
    let event =
      E.map Util_json.(List.to_yojson Stream.to_yojson)
      @@ React.S.changes api.notifs.outgoing_streams in
    Lwt.return (`Ev event)

  let get_mode (api : Protocol.api) _user _body _env _state =
    let event =
      E.map (fun (x : config) ->
          Util_json.List.to_yojson udp_mode_to_yojson x.mode.udp)
      @@ React.S.changes api.notifs.config in
    Lwt.return (`Ev event)

end

let get_status (api : Protocol.api) _user _body _env _state =
  match React.S.value api.notifs.state with
  | `No_response | `Init ->
    let error = Request.error_to_string Not_responding in
    Lwt.return (`Error error)
  | `Fine ->
    Lwt.pick
      [ Protocol.await_no_response api.notifs.state
      ; (Util_react.E.next api.notifs.transmitter_status
         >>= fun x -> Lwt.return_ok x)
      ]
    >>= function
    | Error e -> Lwt.return (`Error (Request.error_to_string e))
    | Ok x -> Lwt.return (`Value (transmitter_status_to_yojson x))

let get_incoming_streams (api : Protocol.api) _user _body _env _state =
  let streams = React.S.value api.notifs.incoming_streams in
  Lwt.return (`Value Util_json.(List.to_yojson Stream.to_yojson streams))

let get_outgoing_streams (api : Protocol.api) _user _body _env _state =
  let streams = React.S.value api.notifs.outgoing_streams in
  Lwt.return (`Value Util_json.(List.to_yojson Stream.to_yojson streams))

let set_streams (api : Protocol.api) _user body _env _state =
  match Util_json.List.of_yojson Stream.of_yojson body with
  | Error e -> Lwt.return (`Error e)
  | Ok streams ->
    let devinfo = React.S.value api.notifs.devinfo in
    match Streams_setup.simple api.ports devinfo streams with
    | Error e -> Lwt.return (`Error (Stream.Table.set_error_to_string e))
    | Ok udp ->
      api.kv#get
      >>= fun config ->
      let mode = { config.mode with udp } in
      let main, aux_1, aux_2 = Request.split_mode mode in
      Lwt_result.Infix.(
        api.channel (Request.Set_mode_main main)
        >>= fun () -> api.channel (Request.Set_mode_aux_1 aux_1)
        >>= fun () -> api.channel (Request.Set_mode_aux_2 aux_2)
        >>= fun () -> Lwt.return_ok mode)
      >>= function
      | Error e -> Lwt.return (`Error (Request.error_to_string e))
      | Ok mode ->
        api.kv#set { config with mode }
        >>= fun () -> Lwt.return `Unit

let set_mode (api : Protocol.api) _user body _env _state =
  match Util_json.List.of_yojson udp_mode_of_yojson body with
  | Error e -> print_endline e; Lwt.return (`Error e)
  | Ok mode ->
    let devinfo = React.S.value api.notifs.devinfo in
    match Streams_setup.full devinfo mode with
    | Error e -> Lwt.return (`Error (Stream.Table.set_error_to_string e))
    | Ok udp ->
      api.kv#get
      >>= fun config ->
      let mode = { config.mode with udp } in
      let main, aux_1, aux_2 = Request.split_mode mode in
      print_endline
      @@ Yojson.Safe.pretty_to_string
      @@ Util_json.List.to_yojson udp_mode_to_yojson main.udp;
      Lwt_result.Infix.(
        api.channel (Request.Set_mode_main main)
        >>= fun () -> api.channel (Request.Set_mode_aux_1 aux_1)
        >>= fun () -> api.channel (Request.Set_mode_aux_2 aux_2)
        >>= fun () -> Lwt.return_ok mode)
      >>= function
      | Error e -> Lwt.return (`Error (Request.error_to_string e))
      | Ok mode ->
        api.kv#set { config with mode }
        >>= fun () -> Lwt.return `Unit
