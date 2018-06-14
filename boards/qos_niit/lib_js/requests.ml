open Containers
open Board_types
open Api_js.Requests.Json_request
open Lwt.Infix

include Boards_js.Requests

let to_unit = fun _ -> Ok ()

let post_reset control () =
  let path = Printf.sprintf "api/board/%d/reset" control in
  post_result ~path to_unit ()

let post_mode control mode =
  let path = Printf.sprintf "api/board/%d/mode" control in
  post_result ~path ~contents:(mode_to_yojson mode) to_unit ()

let post_input control inp =
  let path = Printf.sprintf "api/board/%d/input" control in
  post_result ~path ~contents:(input_to_yojson inp) to_unit ()

let post_t2mi_mode control mode =
  let path = Printf.sprintf "api/board/%d/t2mi_mode" control in
  post_result ~path ~contents:(t2mi_mode_request_to_yojson mode) to_unit ()

let post_jitter_mode control mode =
  let path = Printf.sprintf "api/board/%d/jitter_mode" control in
  post_result ~path ~contents:(jitter_mode_request_to_yojson mode) to_unit ()

let get_config control =
  let path = Printf.sprintf "api/board/%d/config" control in
  get_result ~path config_of_yojson ()

let get_devinfo control =
  let path = Printf.sprintf "api/board/%d/devinfo" control in
  get_result ~path devinfo_response_of_yojson ()

let get_t2mi_seq control seconds =
  let path = Printf.sprintf "api/board/%d/t2mi_seq/%d" control seconds in
  get_result ~path t2mi_seq_of_yojson ()

let get_structs control =
  let path = Printf.sprintf "api/board/%d/structs" control in
  get_result ~path ts_structs_of_yojson ()

let get_bitrates control =
  let path = Printf.sprintf "api/board/%d/bitrates" control in
  get_result ~path ts_structs_of_yojson ()

let get_incoming_streams control =
  let path = Printf.sprintf "api/board/%d/incoming_streams" control in
  get_result ~path Common.Stream.t_list_of_yojson ()

let get_config_ws control =
  let path = Printf.sprintf "api/board/%d/config_ws" control in
  WS.get ~path config_of_yojson ()

let get_status_ws control =
  let path = Printf.sprintf "api/board/%d/status_ws" control in
  WS.get ~path user_status_of_yojson ()

let get_ts_errors_ws control =
  let path = Printf.sprintf "api/board/%d/ts_errors_ws" control in
  WS.get ~path ts_errors_of_yojson ()

let get_t2mi_errors_ws control =
  let path = Printf.sprintf "api/board/%d/t2mi_errors_ws" control in
  WS.get ~path t2mi_errors_of_yojson ()

let get_board_errors_ws control =
  let path = Printf.sprintf "api/board/%d/board_errors_ws" control in
  WS.get ~path board_error_of_yojson ()

let get_bitrates_ws control =
  let path = Printf.sprintf "api/board/%d/bitrate_ws" control in
  WS.get ~path ts_structs_of_yojson ()

let get_structs_ws control =
  let path = Printf.sprintf "api/board/%d/structs_ws" control in
  WS.get ~path ts_structs_of_yojson ()

let get_t2mi_info_ws control =
  let path = Printf.sprintf "api/board/%d/t2mi_info_ws" control in
  WS.get ~path t2mi_info_of_yojson ()

let get_jitter_ws control =
  let path = Printf.sprintf "api/board/%d/jitter_ws" control in
  WS.get ~path jitter_of_yojson ()

let get_incoming_streams_ws control =
  let path = Printf.sprintf "api/board/%d/incoming_streams" control in
  WS.get ~path Common.Stream.t_list_of_yojson ()
