open Board_types
open Api_js.Requests
open Lwt.Infix

let get_devinfo control =
  get_js (Printf.sprintf "/api/board/%d/devinfo" control)
  >|= CCResult.(flat_map devinfo_of_yojson)

let get_config control =
  get_js (Printf.sprintf "/api/board/%d/config" control)
  >|= CCResult.(flat_map config_of_yojson)

let get_state control =
  get_js (Printf.sprintf "/api/board/%d/state" control)
  >|= CCResult.(flat_map Common.Topology.state_of_yojson)

let post_factory_settings control settings =
  factory_settings_to_yojson settings
  |> post_js_ok (Printf.sprintf "/api/board/%d/factory_settings" control)

let post_settings control settings =
  Common.Stream.t_list_to_yojson settings
  |> post_js_ok (Printf.sprintf "/api/board/%d/settings" control)

let get_state_ws control =
  get_socket (Printf.sprintf "api/board/%d/state_ws" control) Common.Topology.state_of_yojson

let get_config_ws control =
  get_socket (Printf.sprintf "api/board/%d/config_ws" control) config_of_yojson

let get_status_ws control =
  get_socket (Printf.sprintf "api/board/%d/status_ws" control) status_of_yojson
