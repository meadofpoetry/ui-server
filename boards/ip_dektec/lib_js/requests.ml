open Containers
open Board_types
open Api_js.Requests
open Lwt.Infix

include Boards_js.Requests

let post_address control addr =
  addr_to_yojson addr
  |> post_js (Printf.sprintf "/api/board/%d/address" control)
  >|= Result.(flat_map addr_of_yojson)

let post_mask control mask =
  mask_to_yojson mask
  |> post_js (Printf.sprintf "/api/board/%d/mask" control)
  >|= Result.(flat_map mask_of_yojson)

let post_gateway control gateway =
  gateway_to_yojson gateway
  |> post_js (Printf.sprintf "/api/board/%d/gateway" control)
  >|= Result.(flat_map gateway_of_yojson)

let post_dhcp control dhcp =
  flag_to_yojson dhcp
  |> post_js (Printf.sprintf "/api/board/%d/dhcp" control)
  >|= Result.(flat_map flag_of_yojson)

let post_ip_enable control enable =
  flag_to_yojson enable
  |> post_js (Printf.sprintf "/api/board/%d/enable" control)
  >|= Result.(flat_map flag_of_yojson)

let post_fec control fec =
  flag_to_yojson fec
  |> post_js (Printf.sprintf "/api/board/%d/fec" control)
  >|= Result.(flat_map flag_of_yojson)

let post_port control port =
  port_to_yojson port
  |> post_js (Printf.sprintf "/api/board/%d/port" control)
  >|= Result.(flat_map port_of_yojson)

let post_meth control meth =
  meth_to_yojson meth
  |> post_js (Printf.sprintf "/api/board/%d/meth" control)
  >|= Result.(flat_map meth_of_yojson)

let post_multicast control addr =
  multicast_to_yojson addr
  |> post_js (Printf.sprintf "/api/board/%d/multicast" control)
  >|= Result.(flat_map multicast_of_yojson)

let post_delay control delay =
  delay_to_yojson delay
  |> post_js (Printf.sprintf "/api/board/%d/delay" control)
  >|= Result.(flat_map delay_of_yojson)

let post_rate_mode control mode =
  rate_mode_to_yojson mode
  |> post_js (Printf.sprintf "/api/board/%d/rate_mode" control)
  >|= Result.(flat_map rate_mode_of_yojson)

let post_reset control =
  post_ok (Printf.sprintf "/api/board/%d/reset" control)

let get_config control =
  get_js (Printf.sprintf "/api/board/%d/config" control)
  >|= Result.(flat_map config_of_yojson)

let get_devinfo control =
  get_js (Printf.sprintf "/api/board/%d/devinfo" control)
  >|= Result.(flat_map devinfo_of_yojson)

let get_status_ws control =
  get_socket (Printf.sprintf "api/board/%d/status_ws" control) board_status_of_yojson

let get_config_ws control =
  get_socket (Printf.sprintf "api/board/%d/config_ws" control) config_of_yojson
