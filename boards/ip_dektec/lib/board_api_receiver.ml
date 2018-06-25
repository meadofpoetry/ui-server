open Containers
open Common
open Board_types
open Board_protocol
open Board_api_common
open Api.Interaction
open Api.Redirect

(** API
    POST /receiver/enable
    POST /receiver/fec
    POST /receiver/port
    POST /receiver/method
    POST /receiver/multicast
    POST /receiver/delay
    POST /receiver/rate-mode
    POST /receiver/mode

    GET  /receiver/enable
    GET  /receiver/fec
    GET  /receiver/port
    GET  /receiver/meth
    GET  /receiver/multicast
    GET  /receiver/delay
    GET  /receiver/rate-mode
    GET  /receiver/mode
    GET  /receiver/status
 *)

module WS = struct

  let map ~eq f e = React.E.changes ~eq @@ React.E.map f e

  let enable sock_data (events:events) body () =
    let e = map ~eq:equal_flag (fun c -> c.ip.enable) events.config in
    sock_handler sock_data e flag_to_yojson body

  let fec sock_data (events:events) body () =
    let e = map ~eq:equal_flag (fun c -> c.ip.fec) events.config in
    sock_handler sock_data e flag_to_yojson body

  let port sock_data (events:events) body () =
    let e = map ~eq:equal_port (fun c -> c.ip.port) events.config in
    sock_handler sock_data e port_to_yojson body

  let meth sock_data (events:events) body () =
    let e = map ~eq:equal_meth (fun c -> match c.ip.multicast with
                                         | Some _ -> Multicast
                                         | None   -> Unicast) events.config in
    sock_handler sock_data e meth_to_yojson body

  let multicast sock_data (events:events) body () =
    let e = map ~eq:(Equal.option Ipaddr.V4.equal) (fun c -> c.ip.multicast) events.config in
    sock_handler sock_data e ipv4_opt_to_yojson body

  let delay sock_data (events:events) body () =
    let e = map ~eq:equal_delay (fun c -> c.ip.delay) events.config in
    sock_handler sock_data e delay_to_yojson body

  let rate_mode sock_data (events:events) body () =
    let e = map ~eq:equal_rate_mode (fun c -> c.ip.rate_mode) events.config in
    sock_handler sock_data e rate_mode_to_yojson body

  let mode sock_data (events:events) body () =
    let e = map ~eq:equal_ip (fun c -> c.ip) events.config in
    sock_handler sock_data e ip_to_yojson body

  let status sock_data (events:events) body () =
    sock_handler sock_data events.status status_to_yojson body

end

module HTTP = struct

  let post setter _of _to body =
    Json.of_body body >>= fun json ->
    (match _of json with
     | Error e -> Lwt_result.fail @@ Json.of_error_string e
     | Ok ip   -> setter ip >|= (_to %> Result.return))
    >>= Json.respond_result

  let post_enable (api:api) body () =
    post api.post_enable flag_of_yojson flag_to_yojson body

  let post_fec (api:api) body () =
    post api.post_fec flag_of_yojson flag_to_yojson body

  let post_port (api:api) body () =
    post api.post_port port_of_yojson port_to_yojson body

  let post_meth (api:api) body () =
    post api.post_meth meth_of_yojson meth_to_yojson body

  let post_multicast (api:api) body () =
    post api.post_multicast Ipaddr.V4.of_yojson Ipaddr.V4.to_yojson body

  let post_delay (api:api) body () =
    post api.post_delay delay_of_yojson delay_to_yojson body

  let post_rate_mode (api:api) body () =
    post api.post_rate_mode rate_mode_of_yojson rate_mode_to_yojson body

  let ip_setter (api:api) (ip:Board_types.ip) =
    let open Lwt.Infix in
    api.post_enable ip.enable
    >>= fun enable    -> api.post_fec ip.fec
    >>= fun fec       -> api.post_port ip.port
    >>= fun port      -> api.post_meth (match ip.multicast with Some _ -> Multicast | None -> Unicast)
    >>= fun meth      -> (match meth,ip.multicast with
                          | Multicast,Some mcast -> Lwt.map Option.return (api.post_multicast mcast)
                          | Unicast,None         -> Lwt.return None
                          | (Multicast,None) | (Unicast,Some _) -> Lwt.fail_invalid_arg "bad meth")
    >>= fun multicast -> api.post_delay ip.delay
    >>= fun delay     -> api.post_rate_mode ip.rate_mode
    >>= fun rate_mode -> Lwt.return { enable; fec; port; multicast; delay; rate_mode }

  let post_mode (api:api) body () =
    post (ip_setter api) ip_of_yojson ip_to_yojson body

  let get (api:api) getter _to =
    api.get_config () >|= (fun (c:config) -> _to (getter c.ip) |> Result.return)
    >>= Json.respond_result

  let get_enable_now (api:api) () =
    get api (fun x -> x.enable) flag_to_yojson
  let get_fec_now (api:api) () =
    get api (fun x -> x.fec) flag_to_yojson
  let get_port_now (api:api) () =
    get api (fun x -> x.port) port_to_yojson
  let get_meth_now (api:api) () =
    get api (fun x -> match x.multicast with Some _ -> Multicast | None -> Unicast) meth_to_yojson
  let get_multicast_now (api:api) () =
    get api (fun x -> x.multicast) ipv4_opt_to_yojson
  let get_delay_now (api:api) () =
    get api (fun x -> x.delay) delay_to_yojson
  let get_rate_mode_now (api:api) () =
    get api (fun x -> x.rate_mode) rate_mode_to_yojson
  let get_mode_now (api:api) () =
    get api (fun x -> x) ip_to_yojson

  module Archive = struct

    let get_status time (query:Uri.Query.t) () =
      not_found ()

  end

  let get_status (query:Uri.Query.t) () =
    match Api.Query.Time.get' query with
    | Ok (Some time,query) -> Archive.get_status time query ()
    | Ok (None,query)      -> not_found ()
    | Error e              -> respond_error "bad query" ()

end

let handler api events id meth ({path;query;_}:Uri.sep) sock_data headers body =
  let is_guest = Common.User.eq id `Guest in
  match Api.Headers.is_ws headers,meth,path with
  (* WS *)
  | true, `GET, ["enable"]    -> WS.enable sock_data events body ()
  | true, `GET, ["fec"]       -> WS.fec sock_data events body ()
  | true, `GET, ["port"]      -> WS.port sock_data events body ()
  | true, `GET, ["method"]    -> WS.meth sock_data events body ()
  | true, `GET, ["multicast"] -> WS.multicast sock_data events body ()
  | true, `GET, ["delay"]     -> WS.delay sock_data events body ()
  | true, `GET, ["rate-mode"] -> WS.rate_mode sock_data events body ()
  | true, `GET, ["mode"]      -> WS.mode sock_data events body ()
  | true, `GET, ["status"]    -> WS.status sock_data events body ()
  (* HTTP *)
  | false,`POST,["enable"]    -> redirect_if is_guest @@ HTTP.post_enable api body
  | false,`POST,["fec"]       -> redirect_if is_guest @@ HTTP.post_fec api body
  | false,`POST,["port"]      -> redirect_if is_guest @@ HTTP.post_port api body
  | false,`POST,["method"]    -> redirect_if is_guest @@ HTTP.post_meth api body
  | false,`POST,["multicast"] -> redirect_if is_guest @@ HTTP.post_multicast api body
  | false,`POST,["delay"]     -> redirect_if is_guest @@ HTTP.post_delay api body
  | false,`POST,["rate-mode"] -> redirect_if is_guest @@ HTTP.post_rate_mode api body
  | false,`POST,["mode"]      -> redirect_if is_guest @@ HTTP.post_mode api body

  | false,`GET, ["enable"]    -> HTTP.get_enable_now api ()
  | false,`GET, ["fec"]       -> HTTP.get_fec_now api ()
  | false,`GET, ["port"]      -> HTTP.get_port_now api ()
  | false,`GET, ["method"]    -> HTTP.get_meth_now api ()
  | false,`GET, ["multicast"] -> HTTP.get_multicast_now api ()
  | false,`GET, ["delay"]     -> HTTP.get_delay_now api ()
  | false,`GET, ["rate-mode"] -> HTTP.get_rate_mode_now api ()
  | false,`GET, ["mode"]      -> HTTP.get_mode_now api ()
  | false,`GET, ["status"]    -> HTTP.get_status query ()
  | _ -> not_found ()
