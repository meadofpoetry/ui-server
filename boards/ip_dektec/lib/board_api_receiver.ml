open Containers
open Board_types
open Board_protocol
open Board_api_common
open Api.Interaction
open Api.Interaction.Json
open Api.Redirect
open Common

module WS = struct

  let map ~eq f e  = React.E.changes ~eq @@ React.E.map f e
  let fmap ~eq f e = React.E.changes ~eq @@ React.E.fmap f e

  let enabled (events:events) _ body sock_data () =
    let e = map ~eq:Equal.bool (fun c -> c.ip.enable) events.config in
    Api.Socket.handler socket_table sock_data e Json.Bool.to_yojson body

  let fec (events:events) _ body sock_data () =
    let e = map ~eq:Equal.bool (fun c -> c.ip.fec) events.config in
    Api.Socket.handler socket_table sock_data e Json.Bool.to_yojson body

  let port (events:events) _ body sock_data () =
    let e = map ~eq:(=) (fun c -> c.ip.port) events.config in
    Api.Socket.handler socket_table sock_data e Json.Int.to_yojson body

  let meth (events:events) _ body sock_data () =
    let e = map ~eq:equal_meth (fun c -> match c.ip.multicast with
                                         | Some _ -> Multicast
                                         | None   -> Unicast) events.config in
    Api.Socket.handler socket_table sock_data e meth_to_yojson body

  let multicast (events:events) _ body sock_data () =
    let e = fmap ~eq:Ipaddr.V4.equal (fun c -> c.ip.multicast) events.config in
    Api.Socket.handler socket_table sock_data e Ipaddr.V4.to_yojson body

  let delay (events:events) _ body sock_data () =
    let e = map ~eq:(=) (fun c -> c.ip.delay) events.config in
    Api.Socket.handler socket_table sock_data e Json.Int.to_yojson body

  let rate_mode (events:events) _ body sock_data () =
    let e = map ~eq:equal_rate_mode (fun c -> c.ip.rate_mode) events.config in
    Api.Socket.handler socket_table sock_data e rate_mode_to_yojson body

  let mode (events:events) _ body sock_data () =
    let e = map ~eq:equal_ip (fun c -> c.ip) events.config in
    Api.Socket.handler socket_table sock_data e ip_to_yojson body

  let status (events:events) _ body sock_data () =
    Api.Socket.handler socket_table sock_data events.status status_to_yojson body

end

module HTTP = struct

  let set setter _of _to body =
    of_body body >>= fun json ->
    (match _of json with
     | Error e -> Lwt_result.fail @@ of_error_string e
     | Ok ip   -> setter ip >|= (_to %> Result.return))
    >>= respond_result

  let set_enable    (api:api) _ body () = set api.set_enable Json.Bool.of_yojson Json.Bool.to_yojson body
  let set_fec       (api:api) _ body () = set api.set_fec Json.Bool.of_yojson Json.Bool.to_yojson body
  let set_port      (api:api) _ body () = set api.set_port Json.Int.of_yojson Json.Int.to_yojson body
  let set_meth      (api:api) _ body () = set api.set_meth meth_of_yojson meth_to_yojson body
  let set_multicast (api:api) _ body () = set api.set_multicast Ipaddr.V4.of_yojson
                                            Ipaddr.V4.to_yojson body
  let set_delay     (api:api) _ body () = set api.set_delay Json.Int.of_yojson Json.Int.to_yojson body
  let set_rate_mode (api:api) _ body () = set api.set_rate_mode rate_mode_of_yojson rate_mode_to_yojson body

  let ip_setter (api:api) (ip:Board_types.ip) =
    let open Lwt.Infix in
    api.set_enable ip.enable
    >>= fun enable    -> api.set_fec ip.fec
    >>= fun fec       -> api.set_port ip.port
    >>= fun port      -> api.set_meth (match ip.multicast with Some _ -> Multicast | None -> Unicast)
    >>= fun meth      -> (match meth,ip.multicast with
                          | Multicast,Some mcast -> Lwt.map Option.return (api.set_multicast mcast)
                          | Unicast,None         -> Lwt.return None
                          | (Multicast,None) | (Unicast,Some _) -> Lwt.fail_invalid_arg "bad meth")
    >>= fun multicast -> api.set_delay ip.delay
    >>= fun delay     -> api.set_rate_mode ip.rate_mode
    >>= fun rate_mode -> Lwt.return { enable; fec; port; multicast; delay; rate_mode }

  let set_mode (api:api) _ body () =
    set (ip_setter api) ip_of_yojson ip_to_yojson body

  let get (api:api) getter to_yojson =
    getter (api.get_config ()).ip |> to_yojson |> Result.return |> respond_result

  let get_enable    (api:api) _ _ () = get api (fun x -> x.enable) Json.Bool.to_yojson
  let get_fec       (api:api) _ _ () = get api (fun x -> x.fec) Json.Bool.to_yojson
  let get_port      (api:api) _ _ () = get api (fun x -> x.port) Json.Int.to_yojson
  let get_meth      (api:api) _ _ () = get api (fun x -> match x.multicast with
                                                         | Some _ -> Multicast
                                                         | None -> Unicast) meth_to_yojson
  let get_multicast (api:api) _ _ () = get api (fun x -> x.multicast)
                                         (Json.Option.to_yojson Ipaddr.V4.to_yojson)
  let get_delay     (api:api) _ _ () = get api (fun x -> x.delay) Json.Int.to_yojson
  let get_rate_mode (api:api) _ _ () = get api (fun x -> x.rate_mode) rate_mode_to_yojson
  let get_mode      (api:api) _ _ () = get api (fun x -> x) ip_to_yojson

  let get_status (api:api) _ _ () =
    api.get_status () |> Json.Option.to_yojson status_to_yojson
    |> Result.return |> respond_result

  module Archive = struct

    let get_status limit from compress till duration _ _ () =
      not_found ()

    let get_mode limit from till duration _ _ () =
      not_found ()

  end

end

let handler api events =
  let open Uri in
  let open Boards.Board.Api_handler in
  create_dispatcher
    "receiver"
    [ create_ws_handler ~docstring:"Notifies client when receiver is enabled/disabled"
        ~path:Path.Format.("enabled" @/ empty)
        ~query:Query.empty
        (WS.enabled events)
    ; create_ws_handler ~docstring:"Notifies client when receiver FEC mode is changed"
        ~path:Path.Format.("fec" @/ empty)
        ~query:Query.empty
        (WS.fec events)
    ; create_ws_handler ~docstring:"Notifies client when receiver UDP port is changed"
        ~path:Path.Format.("port" @/ empty)
        ~query:Query.empty
        (WS.port events)
    ; create_ws_handler ~docstring:"Notifies client when receive method (Unicast/Multicast) is changed"
        ~path:Path.Format.("method" @/ empty)
        ~query:Query.empty
        (WS.meth events)
    ; create_ws_handler ~docstring:"Notifies client when receiver multicast address is changed"
        ~path:Path.Format.("multicast" @/ empty)
        ~query:Query.empty
        (WS.multicast events)
    ; create_ws_handler ~docstring:"Notifies client when receiver IP-to-output delay is changed"
        ~path:Path.Format.("delay" @/ empty)
        ~query:Query.empty
        (WS.delay events)
    ; create_ws_handler ~docstring:"Notifies client when receiver rate estimation mode is changed"
        ~path:Path.Format.("rate-mode" @/ empty)
        ~query:Query.empty
        (WS.rate_mode events)
    ; create_ws_handler ~docstring:"Notifies client when receiver mode is changed"
        ~path:Path.Format.("mode" @/ empty)
        ~query:Query.empty
        (WS.mode events)
    ; create_ws_handler ~docstring:"Pushes receiver status to the client"
        ~path:Path.Format.("status" @/ empty)
        ~query:Query.empty
        (WS.status events)
    ]
    [ `POST, [ create_handler ~docstring:"Enables/disables the receiver"
                 ~restrict:[ `Guest ]
                 ~path:Path.Format.("enabled" @/ empty)
                 ~query:Query.empty
                 (HTTP.set_enable api)
             ; create_handler ~docstring:"Enables/disables FEC"
                 ~restrict:[ `Guest ]
                 ~path:Path.Format.("fec" @/ empty)
                 ~query:Query.empty
                 (HTTP.set_fec api)
             ; create_handler ~docstring:"Sets UDP port"
                 ~restrict:[ `Guest ]
                 ~path:Path.Format.("port" @/ empty)
                 ~query:Query.empty
                 (HTTP.set_port api)
             ; create_handler ~docstring:"Sets receive method (Unicast/Multicast)"
                 ~restrict:[ `Guest ]
                 ~path:Path.Format.("method" @/ empty)
                 ~query:Query.empty
                 (HTTP.set_meth api)
             ; create_handler ~docstring:"Sets Multicast address"
                 ~restrict:[ `Guest ]
                 ~path:Path.Format.("multicast" @/ empty)
                 ~query:Query.empty
                 (HTTP.set_multicast api)
             ; create_handler ~docstring:"Sets IP-to-output delay"
                 ~restrict:[ `Guest ]
                 ~path:Path.Format.("delay" @/ empty)
                 ~query:Query.empty
                 (HTTP.set_delay api)
             ; create_handler ~docstring:"Sets rate estimation mode"
                 ~restrict:[ `Guest ]
                 ~path:Path.Format.("rate-mode" @/ empty)
                 ~query:Query.empty
                 (HTTP.set_rate_mode api)
             ; create_handler ~docstring:"Sets receiver mode"
                 ~restrict:[ `Guest ]
                 ~path:Path.Format.("mode" @/ empty)
                 ~query:Query.empty
                 (HTTP.set_mode api)
             ]
    ; `GET,  [ create_handler ~docstring:"Returns a value indicating whether the receiver is on"
                 ~path:Path.Format.("enabled" @/ empty)
                 ~query:Query.empty
                 (HTTP.get_enable api)
             ; create_handler ~docstring:"Returns a value indicating whether FEC is on"
                 ~path:Path.Format.("fec" @/ empty)
                 ~query:Query.empty
                 (HTTP.get_fec api)
             ; create_handler ~docstring:"Returns current UDP port value"
                 ~path:Path.Format.("port" @/ empty)
                 ~query:Query.empty
                 (HTTP.get_port api)
             ; create_handler ~docstring:"Returns current receive method (Unicast/Multicast)"
                 ~path:Path.Format.("method" @/ empty)
                 ~query:Query.empty
                 (HTTP.get_meth api)
             ; create_handler ~docstring:"Returns current Multicast address or null if not set"
                 ~path:Path.Format.("multicast" @/ empty)
                 ~query:Query.empty
                 (HTTP.get_multicast api)
             ; create_handler ~docstring:"Returns current IP-to-output delay"
                 ~path:Path.Format.("delay" @/ empty)
                 ~query:Query.empty
                 (HTTP.get_delay api)
             ; create_handler ~docstring:"Returns current rate estimation mode"
                 ~path:Path.Format.("rate-mode" @/ empty)
                 ~query:Query.empty
                 (HTTP.get_rate_mode api)
             ; create_handler ~docstring:"Returns current receiver mode"
                 ~path:Path.Format.("mode" @/ empty)
                 ~query:Query.empty
                 (HTTP.get_mode api)
             ; create_handler ~docstring:"Returns last received receiver status"
                 ~path:Path.Format.("status" @/ empty)
                 ~query:Query.empty
                 (HTTP.get_status api)
             (* Archive *)
             ; create_handler ~docstring:"Returns archived receiver status"
                 ~path:Path.Format.("status/archive" @/ empty)
                 ~query:Query.[ "limit",    (module Option(Int))
                              ; "compress", (module Option(Bool))
                              ; "from",     (module Option(Time.Show))
                              ; "to",       (module Option(Time.Show))
                              ; "duration", (module Option(Time.Relative)) ]
                 HTTP.Archive.get_status
             ; create_handler ~docstring:"Returns archived receiver mode"
                 ~path:Path.Format.("mode/archive" @/ empty)
                 ~query:Query.[ "limit",    (module Option(Int))
                              ; "from",     (module Option(Time.Show))
                              ; "to",       (module Option(Time.Show))
                              ; "duration", (module Option(Time.Relative)) ]
                 HTTP.Archive.get_mode
             ]
    ]
