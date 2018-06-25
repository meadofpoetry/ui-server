open Containers
open Common
open Board_types
open Board_protocol
open Board_api_common
open Api.Interaction
open Api.Redirect

(** API
    POST /device/reset
    POST /device/ip
    POST /device/mask
    POST /device/gateway
    POST /device/dhcp
    POST /device/mode

    GET  /device/state
    GET  /device/info
    GET  /device/ip
    GET  /device/mask
    GET  /device/gateway
    GET  /device/dhcp
    GET  /device/mode
 *)

module WS = struct

  let map ~eq f e = React.E.changes ~eq @@ React.E.map f e

  let state sock_data (events:events) body () =
    sock_handler sock_data (React.S.changes events.state) Topology.state_to_yojson body

  let ip sock_data (events:events) body () =
    let e = map ~eq:Ipaddr.V4.equal (fun c -> c.nw.ip) events.config in
    sock_handler sock_data e Ipaddr.V4.to_yojson body

  let mask sock_data (events:events) body () =
    let e = map ~eq:Ipaddr.V4.equal (fun c -> c.nw.mask) events.config in
    sock_handler sock_data e Ipaddr.V4.to_yojson body

  let gateway sock_data (events:events) body () =
    let e = map ~eq:Ipaddr.V4.equal (fun c -> c.nw.gateway) events.config in
    sock_handler sock_data e Ipaddr.V4.to_yojson body

  let dhcp sock_data (events:events) body () =
    let e = map ~eq:equal_flag (fun c -> c.nw.dhcp) events.config in
    sock_handler sock_data e flag_to_yojson body

  let mode sock_data (events:events) body () =
    let e = map ~eq:equal_nw (fun c -> c.nw) events.config in
    sock_handler sock_data e nw_to_yojson body

end

module HTTP = struct

  let post setter _of _to body =
    Json.of_body body >>= fun json ->
    (match _of json with
     | Error e -> Lwt_result.fail @@ Json.of_error_string e
     | Ok ip   -> setter ip >|= (_to %> Result.return))
    >>= Json.respond_result

  let post_reset (api:api) () =
    api.post_reset () >|= Result.return
    >>= Json.respond_result_unit

  let post_ip (api:api) body () =
    post api.post_ip Ipaddr.V4.of_yojson Ipaddr.V4.to_yojson body

  let post_mask (api:api) body () =
    post api.post_mask Ipaddr.V4.of_yojson Ipaddr.V4.to_yojson body

  let post_gateway (api:api) body () =
    post api.post_gateway Ipaddr.V4.of_yojson Ipaddr.V4.to_yojson body

  let post_dhcp (api:api) body () =
    post api.post_dhcp flag_of_yojson flag_to_yojson body

  let nw_setter (api:api) (nw:Board_types.nw) =
    let open Lwt.Infix in
    api.post_ip nw.ip
    >>= fun ip   -> api.post_mask nw.mask
    >>= fun mask -> api.post_gateway nw.gateway
    >>= fun gw   -> api.post_dhcp nw.dhcp
    >>= fun dhcp -> Lwt.return { ip; mask; gateway = gw; dhcp }

  let post_mode (api:api) body () =
    post (nw_setter api) nw_of_yojson nw_to_yojson body

  let get_state_now (events:events) () =
    React.S.value events.state
    |> Topology.state_to_yojson
    |> Result.return
    |> Json.respond_result

  let get_ip_now (api:api) () =
    api.get_config () >|= (fun (c:config) -> Ipaddr.V4.to_yojson c.nw.ip |> Result.return)
    >>= Json.respond_result

  let get_mask_now (api:api) () =
    api.get_config () >|= (fun (c:config) -> Ipaddr.V4.to_yojson c.nw.mask |> Result.return)
    >>= Json.respond_result

  let get_gateway_now (api:api) () =
    api.get_config () >|= (fun (c:config) -> Ipaddr.V4.to_yojson c.nw.gateway |> Result.return)
    >>= Json.respond_result

  let get_dhcp_now (api:api) () =
    api.get_config () >|= (fun (c:config) -> flag_to_yojson c.nw.dhcp |> Result.return)
    >>= Json.respond_result

  let get_mode_now (api:api) () =
    api.get_config () >|= (fun (c:config) -> nw_to_yojson c.nw |> Result.return)
    >>= Json.respond_result

end

let handler api events id meth ({path;_}:Uri.sep) sock_data headers body =
  let is_guest = Common.User.eq id `Guest in
  match Api.Headers.is_ws headers,meth,path with
  (* WS *)
  | true, `GET, ["state"]   -> WS.state sock_data events body ()
  | true, `GET, ["ip"]      -> WS.ip sock_data events body ()
  | true, `GET, ["mask"]    -> WS.mask sock_data events body ()
  | true, `GET, ["gateway"] -> WS.gateway sock_data events body ()
  | true, `GET, ["dhcp"]    -> WS.dhcp sock_data events body ()
  | true, `GET, ["mode"]    -> WS.mode sock_data events body ()
  (* HTTP *)
  | false,`POST,["reset"]   -> redirect_if is_guest @@ HTTP.post_reset api
  | false,`POST,["ip"]      -> redirect_if is_guest @@ HTTP.post_ip api body
  | false,`POST,["mask"]    -> redirect_if is_guest @@ HTTP.post_mask api body
  | false,`POST,["gateway"] -> redirect_if is_guest @@ HTTP.post_gateway api body
  | false,`POST,["dhcp"]    -> redirect_if is_guest @@ HTTP.post_dhcp api body
  | false,`POST,["mode"]    -> redirect_if is_guest @@ HTTP.post_mode api body

  | false,`GET, ["state"]   -> HTTP.get_state_now events ()
  | false,`GET, ["ip"]      -> HTTP.get_ip_now api ()
  | false,`GET, ["mask"]    -> HTTP.get_mask_now api ()
  | false,`GET, ["gateway"] -> HTTP.get_gateway_now api ()
  | false,`GET, ["dhcp"]    -> HTTP.get_dhcp_now api ()
  | false,`GET, ["mode"]    -> HTTP.get_mode_now api ()
  | _ -> not_found ()
