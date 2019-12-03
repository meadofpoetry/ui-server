open Board_niitv_ts2ip_types
open Application_types
open Api_util

let ( >>= ) = Lwt.( >>= )

module Event = struct
  open Util_react

  let get_status (api : Protocol.api) _user =
    let event = E.map transmitter_status_to_yojson api.notifs.transmitter_status in
    Lwt.return event

  let get_mode (api : Protocol.api) _user =
    let event =
      E.map (fun (x : config) -> Util_json.List.to_yojson udp_mode_to_yojson x.mode.udp)
      @@ React.S.changes api.notifs.config
    in
    Lwt.return event
end

let get_status (api : Protocol.api) _user _body _env _state =
  Lwt.pick
    [ Boards.Board.await_no_response api.notifs.state >>= not_responding
    ; Util_react.E.next api.notifs.transmitter_status >>= Lwt.return_ok
    ; Fsm.sleep Fsm.status_timeout ]
  >>=? return_value % transmitter_status_to_yojson

let set_streams (api : Protocol.api) _user body _env _state =
  match Util_json.List.of_yojson Stream.of_yojson body with
  | Error e -> Lwt.return (`Error e)
  | Ok streams -> (
      let devinfo = React.S.value api.notifs.devinfo in
      match Streams_setup.simple api.ports devinfo streams with
      | Error e -> Lwt.return (`Error (Stream.Table.set_error_to_string e))
      | Ok udp ->
          (api.kv)#get
          >>= fun config ->
          let mode = {config.mode with udp} in
          let main, aux_1, aux_2 = Request.split_mode mode in
          Lwt_result.Infix.(
            api.channel (Request.Set_mode_main main)
            >>= fun () ->
            api.channel (Request.Set_mode_aux_1 aux_1)
            >>= fun () ->
            api.channel (Request.Set_mode_aux_2 aux_2) >>= fun () -> Lwt.return_ok mode)
          >>=? fun mode -> (api.kv)#set {config with mode} >>= fun () -> Lwt.return `Unit
      )

let set_mode_ (api : Protocol.api) mode =
  let devinfo = React.S.value api.notifs.devinfo in
  match Streams_setup.full devinfo mode with
  | Error e -> Lwt.return (`Error (Stream.Table.set_error_to_string e))
  | Ok udp ->
      (api.kv)#get
      >>= fun config ->
      let mode = {config.mode with udp} in
      let main, aux_1, aux_2 = Request.split_mode mode in
      Lwt_result.Infix.(
        api.channel (Request.Set_mode_main main)
        >>= fun () ->
        api.channel (Request.Set_mode_aux_1 aux_1)
        >>= fun () ->
        api.channel (Request.Set_mode_aux_2 aux_2) >>= fun () -> Lwt.return_ok mode)
      >>=? fun mode -> (api.kv)#set {config with mode} >>= fun () -> Lwt.return `Unit

let set_mode (api : Protocol.api) _user body _env _state =
  match Util_json.List.of_yojson udp_mode_of_yojson body with
  | Error e -> Lwt.return (`Error e)
  | Ok mode -> set_mode_ api mode
