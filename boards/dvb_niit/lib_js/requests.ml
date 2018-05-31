open Containers
open Board_types
open Api_js.Requests.Json
open Lwt.Infix

include Boards_js.Requests

let get_devinfo control =
  get_result devinfo_response_of_yojson (Printf.sprintf "/api/board/%d/devinfo" control)

let get_plps control num =
  get_result plp_list_response_of_yojson (Printf.sprintf "/api/board/%d/plps/%d" control num)

let get_config control =
  get_result config_of_yojson (Printf.sprintf "/api/board/%d/config" control)

let post_reset control =
  post_result (fun _ -> Ok ()) (Printf.sprintf "/api/board/%d/reset" control)

let post_settings control settings =
  post_result ~contents:(settings_request_to_yojson settings)
              settings_response_of_yojson
              (Printf.sprintf "/api/board/%d/settings" control)

let post_plp_setting control plp_setting =
  post_result ~contents:(plp_setting_request_to_yojson plp_setting)
              plp_setting_response_of_yojson
              (Printf.sprintf "/api/board/%d/plp_setting" control)

let get_config_ws control =
  WS.get (Printf.sprintf "api/board/%d/config_ws" control) config_of_yojson

let get_measures_ws control =
  WS.get (Printf.sprintf "api/board/%d/measures_ws" control) measure_response_of_yojson
