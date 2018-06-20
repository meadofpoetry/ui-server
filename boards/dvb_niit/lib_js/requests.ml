open Containers
open Board_types
open Api_js.Requests.Json_request
open Lwt.Infix

include Boards_js.Requests

let get_devinfo control =
  let path = Printf.sprintf "/api/board/%d/devinfo" control in
  get_result ~path devinfo_opt_of_yojson ()

let get_config control =
  let path = Printf.sprintf "/api/board/%d/config" control in
  get_result ~path config_of_yojson ()

let post_reset control =
  let path = Printf.sprintf "/api/board/%d/reset" control in
  post_result ~path (fun _ -> Ok ()) ()

let post_settings control settings =
  let path = Printf.sprintf "/api/board/%d/settings" control in
  post_result ~path ~contents:(mode_req_to_yojson settings)
              mode_rsp_of_yojson
              ()

let post_plp_setting control plp_setting =
  let path = Printf.sprintf "/api/board/%d/plp_setting" control in
  post_result ~path ~contents:(plp_set_req_to_yojson plp_setting)
              plp_set_rsp_of_yojson
              ()

let get_config_ws control =
  let path = Printf.sprintf "api/board/%d/config_ws" control in
  WS.get ~path config_of_yojson ()

let get_measures_ws control =
  let path = Printf.sprintf "api/board/%d/measures_ws" control in
  WS.get ~path measures_of_yojson ()
