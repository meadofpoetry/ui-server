open Containers
open Board_types
open Api_js.Requests.Json_request
open Lwt.Infix

include Boards_js.Requests

let post_address control addr =
  let path = Printf.sprintf "api/board/%d/address" control in
  post_result ~path ~contents:(addr_to_yojson addr) addr_of_yojson ()

let post_mask control mask =
  let path = Printf.sprintf "api/board/%d/mask" control in
  post_result ~path ~contents:(mask_to_yojson mask) mask_of_yojson ()

let post_gateway control gateway =
  let path = Printf.sprintf "api/board/%d/gateway" control in
  post_result ~path ~contents:(gateway_to_yojson gateway) gateway_of_yojson ()

let post_dhcp control dhcp =
  let path = Printf.sprintf "api/board/%d/dhcp" control in
  post_result ~path ~contents:(flag_to_yojson dhcp) flag_of_yojson ()

let post_ip_enable control enable =
  let path = Printf.sprintf "api/board/%d/enable" control in
  post_result ~path ~contents:(flag_to_yojson enable) flag_of_yojson ()

let post_fec control fec =
  let path = Printf.sprintf "api/board/%d/fec" control in
  post_result ~path ~contents:(flag_to_yojson fec) flag_of_yojson ()

let post_port control port =
  let path = Printf.sprintf "api/board/%d/port" control in
  post_result ~path ~contents:(port_to_yojson port) port_of_yojson ()

let post_meth control meth =
  let path = Printf.sprintf "api/board/%d/meth" control in
  post_result ~path ~contents:(meth_to_yojson meth) meth_of_yojson ()

let post_multicast control addr =
  let path = Printf.sprintf "api/board/%d/multicast" control in
  post_result ~path ~contents:(multicast_to_yojson addr) multicast_of_yojson ()

let post_delay control delay =
  let path = Printf.sprintf "api/board/%d/delay" control in
  post_result ~path ~contents:(delay_to_yojson delay) delay_of_yojson ()

let post_rate_mode control mode =
  let path = Printf.sprintf "api/board/%d/rate_mode" control in
  post_result ~path ~contents:(rate_mode_to_yojson mode) rate_mode_of_yojson ()

let post_reset control =
  let path = Printf.sprintf "/api/board/%d/reset" control in
  post_result ~path (fun _ -> Ok ()) ()

let get_config control =
  let path = Printf.sprintf "/api/board/%d/config" control in
  get_result ~path config_of_yojson ()

let get_devinfo control =
  let path = Printf.sprintf "/api/board/%d/devinfo" control in
  get_result ~path devinfo_of_yojson ()

let get_status_ws control =
  let path = Printf.sprintf "api/board/%d/status_ws" control in
  WS.get ~path board_status_of_yojson ()

let get_config_ws control =
  let path = Printf.sprintf "api/board/%d/config_ws" control in
  WS.get ~path config_of_yojson ()
