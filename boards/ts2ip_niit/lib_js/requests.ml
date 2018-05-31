open Containers
open Board_types
open Api_js.Requests.Json
open Lwt.Infix

include Boards_js.Requests

let to_unit = fun _ -> Ok ()

let post_factory_mode control settings =
  let path = Printf.sprintf "/api/board/%d/factory_mode" control in
  post_result ~contents:(factory_settings_to_yojson settings) to_unit path

let post_nw_mode control settings =
  let path = Printf.sprintf "/api/board/%d/nw_mode" control in
  post_result ~contents:(nw_settings_to_yojson settings) to_unit path

let post_streams_simple control streams =
  let path = Printf.sprintf "/api/board/%d/streams_simple" control in
  post_result ~contents:(Common.Stream.t_list_to_yojson streams) to_unit path

let post_streams_full control streams =
  let path = Printf.sprintf "/api/board/%d/streams_full" control in
  post_result ~contents:(streams_full_request_to_yojson streams) to_unit path

let get_devinfo control =
  get_result devinfo_of_yojson (Printf.sprintf "/api/board/%d/devinfo" control)

let get_config control =
  get_result config_response_of_yojson (Printf.sprintf "/api/board/%d/config" control)

let get_streams control =
  get_result Common.Stream.t_list_of_yojson (Printf.sprintf "/api/board/%d/streams" control)

let get_status_ws control =
  WS.get (Printf.sprintf "api/board/%d/status_ws" control) status_of_yojson

let get_config_ws control =
  WS.get (Printf.sprintf "api/board/%d/config_ws" control) config_response_of_yojson

let get_streams_ws control =
  WS.get (Printf.sprintf "api/board/%d/streams_ws" control) Common.Stream.t_list_of_yojson
