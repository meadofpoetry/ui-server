open Containers
open Board_types
open Api_js.Requests
open Lwt.Infix

let get_devinfo control =
  get_js (Printf.sprintf "/api/board/%d/devinfo" control)
  >|= Result.(flat_map devinfo_response_of_yojson)

let get_plps control num =
  get_js (Printf.sprintf "/api/board/%d/plps/%d" control num)
  >|= Result.(flat_map plp_list_response_of_yojson)

let get_config control =
  get_js (Printf.sprintf "/api/board/%d/config" control)
  >|= Result.(flat_map config_of_yojson)

let get_state control =
  get_js (Printf.sprintf "/api/board/%d/state" control)
  >|= Result.(flat_map Common.Topology.state_of_yojson)

let post_reset control =
  post_ok (Printf.sprintf "/api/board/%d/reset" control)

let post_settings control settings =
  settings_request_to_yojson settings
  |> post_js (Printf.sprintf "/api/board/%d/settings" control)
  >|= Result.(flat_map settings_response_of_yojson)

let post_plp_setting control plp_setting =
  plp_setting_request_to_yojson plp_setting
  |> post_js (Printf.sprintf "/api/board/%d/plp_setting" control)
  >|= Result.(flat_map plp_setting_response_of_yojson)

let get_state_ws control =
  get_socket (Printf.sprintf "api/board/%d/state_ws" control) Common.Topology.state_of_yojson

let get_config_ws control =
  get_socket (Printf.sprintf "api/board/%d/config_ws" control) config_of_yojson

let get_measures_ws control =
  get_socket (Printf.sprintf "api/board/%d/measures_ws" control) measure_response_of_yojson
