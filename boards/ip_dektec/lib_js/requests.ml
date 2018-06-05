open Containers
open Board_types
open Api_js.Requests.Json
open Lwt.Infix

include Boards_js.Requests

let post_address control addr =
  post_result ~contents:(addr_to_yojson addr) addr_of_yojson (Printf.sprintf "/api/board/%d/address" control)

let post_mask control mask =
  post_result ~contents:(mask_to_yojson mask) mask_of_yojson (Printf.sprintf "/api/board/%d/mask" control)

let post_gateway control gateway =
  let path = Printf.sprintf "/api/board/%d/gateway" control in
  post_result ~contents:(gateway_to_yojson gateway) gateway_of_yojson path

let post_dhcp control dhcp =
  post_result ~contents:(flag_to_yojson dhcp) flag_of_yojson (Printf.sprintf "/api/board/%d/dhcp" control)

let post_ip_enable control enable =
  post_result ~contents:(flag_to_yojson enable) flag_of_yojson (Printf.sprintf "/api/board/%d/enable" control)

let post_fec control fec =
  post_result ~contents:(flag_to_yojson fec) flag_of_yojson (Printf.sprintf "/api/board/%d/fec" control)

let post_port control port =
  post_result ~contents:(port_to_yojson port) port_of_yojson (Printf.sprintf "/api/board/%d/port" control)

let post_meth control meth =
  post_result ~contents:(meth_to_yojson meth) meth_of_yojson (Printf.sprintf "/api/board/%d/meth" control)

let post_multicast control addr =
  let path = Printf.sprintf "/api/board/%d/multicast" control in
  post_result ~contents:(multicast_to_yojson addr) multicast_of_yojson path

let post_delay control delay =
  post_result ~contents:(delay_to_yojson delay) delay_of_yojson (Printf.sprintf "/api/board/%d/delay" control)

let post_rate_mode control mode =
  let path = Printf.sprintf "/api/board/%d/rate_mode" control in
  post_result ~contents:(rate_mode_to_yojson mode) rate_mode_of_yojson path

let post_reset control =
  post_result (fun _ -> Ok ()) (Printf.sprintf "/api/board/%d/reset" control)

let get_config control =
  get_result config_of_yojson (Printf.sprintf "/api/board/%d/config" control)

let get_devinfo control =
  get_result devinfo_of_yojson (Printf.sprintf "/api/board/%d/devinfo" control)

let get_status_ws control =
  WS.get (Printf.sprintf "api/board/%d/status_ws" control) board_status_of_yojson

let get_config_ws control =
  WS.get (Printf.sprintf "api/board/%d/config_ws" control) config_of_yojson
