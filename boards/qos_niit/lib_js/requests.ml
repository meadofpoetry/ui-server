open Board_types
open Api_js.Requests
open Lwt.Infix

let post_reset control () =
  post_ok (Printf.sprintf "/api/board/%d/reset" control)

let post_mode control mode =
  mode_to_yojson mode
  |> post_js_ok (Printf.sprintf "/api/board/%d/mode" control)

let post_jitter_mode control mode =
  jitter_mode_to_yojson mode
  |> post_js_ok (Printf.sprintf "/api/board/%d/jitter_mode" control)

let get_config control =
  get_js (Printf.sprintf "/api/board/%d/config" control)
  >|= CCResult.(flat_map config_of_yojson)

let get_devinfo control =
  get_js (Printf.sprintf "/api/board/%d/devinfo" control)
  >|= CCResult.(flat_map devinfo_response_of_yojson)

let get_t2mi_seq control seconds =
  get_js (Printf.sprintf "/api/board/%d/t2mi_seq/%d" control seconds)
  >|= CCResult.(flat_map t2mi_seq_of_yojson)

let get_structs control =
  get_js (Printf.sprintf "/api/board/%d/structs" control)
  >|= CCResult.(flat_map ts_structs_of_yojson)

let get_bitrates control =
  get_js (Printf.sprintf "/api/board/%d/bitrates" control)
  >|= CCResult.(flat_map ts_structs_of_yojson)

let get_state_ws control =
  get_socket (Printf.sprintf "api/board/%d/state_ws" control) Common.Topology.state_of_yojson

let get_status_ws control =
  get_socket (Printf.sprintf "api/board/%d/status_ws" control) user_status_of_yojson

let get_ts_errors_ws control =
  get_socket (Printf.sprintf "api/board/%d/ts_errors_ws" control) ts_errors_of_yojson

let get_t2mi_errors_ws control =
  get_socket (Printf.sprintf "api/board/%d/t2mi_errors_ws" control) t2mi_errors_of_yojson

let get_board_errors_ws control =
  get_socket (Printf.sprintf "api/board/%d/board_errors_ws" control) board_error_of_yojson

let get_bitrate_ws control =
  get_socket (Printf.sprintf "api/board/%d/bitrate_ws" control) ts_structs_of_yojson

let get_structs_ws control =
  get_socket (Printf.sprintf "api/board/%d/structs_ws" control) ts_structs_of_yojson

let get_t2mi_info_ws control =
  get_socket (Printf.sprintf "api/board/%d/t2mi_info_ws" control) t2mi_info_of_yojson

let get_jitter_ws control =
  get_socket (Printf.sprintf "api/board/%d/jitter_ws" control) jitter_of_yojson
