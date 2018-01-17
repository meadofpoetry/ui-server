open Board_types
open Api_js.Requests
open Lwt.Infix

let post_factory_mode control settings =
  factory_settings_to_yojson settings
  |> post_js_ok (Printf.sprintf "/api/board/%d/factory_mode" control)

let post_nw_mode control settings =
  nw_settings_to_yojson settings
  |> post_js_ok (Printf.sprintf "/api/board/%d/nw_mode" control)

let post_streams_simple control streams =
  Common.Stream.t_list_to_yojson streams
  |> post_js_ok (Printf.sprintf "/api/board/%d/streams_simple" control)

let post_streams_full control streams =
  streams_full_request_to_yojson streams
  |> post_js_ok (Printf.sprintf "/api/board/%d/streams_full" control)


let get_devinfo control =
  get_js (Printf.sprintf "/api/board/%d/devinfo" control)
  >|= CCResult.(flat_map devinfo_of_yojson)

let get_state control =
  get_js (Printf.sprintf "/api/board/%d/state" control)
  >|= CCResult.(flat_map Common.Topology.state_of_yojson)

let get_config control =
  get_js (Printf.sprintf "/api/board/%d/config" control)
  >|= CCResult.(flat_map config_of_yojson)

let get_streams control =
  get_js (Printf.sprintf "/api/board/%d/streams" control)
  >|= CCResult.(flat_map Common.Stream.t_list_of_yojson)


let get_state_ws control =
  get_socket (Printf.sprintf "api/board/%d/state_ws" control) Common.Topology.state_of_yojson

let get_status_ws control =
  get_socket (Printf.sprintf "api/board/%d/status_ws" control) status_of_yojson

let get_config_ws control =
  get_socket (Printf.sprintf "api/board/%d/config_ws" control) config_response_of_yojson

let get_streams_ws control =
  get_socket (Printf.sprintf "api/board/%d/streams_ws" control) Common.Stream.t_list_of_yojson
