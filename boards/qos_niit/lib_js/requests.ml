open Containers
open Board_types
open Structure_types
open Api_js.Requests.Json_request
open Lwt.Infix

include Boards_js.Requests

type ('a,'b) rsp = ('a,'b Api_js.Requests.err) Lwt_result.t

let to_unit = fun _ -> Ok ()

let post_reset control () =
  post_result to_unit (Printf.sprintf "/api/board/%d/reset" control)

let post_input control inp =
  post_result ~contents:(input_to_yojson inp) to_unit (Printf.sprintf "/api/board/%d/input" control)

let post_t2mi_mode control mode =
  let path = Printf.sprintf "/api/board/%d/t2mi_mode" control in
  post_result ~contents:(t2mi_mode_request_to_yojson mode) to_unit path

let post_jitter_mode control mode =
  let path = Printf.sprintf "/api/board/%d/jitter_mode" control in
  post_result ~contents:(jitter_mode_request_to_yojson mode) to_unit path

let get_config control =
  get_result config_of_yojson (Printf.sprintf "/api/board/%d/config" control)

let get_devinfo control =
  get_result devinfo_response_of_yojson (Printf.sprintf "/api/board/%d/devinfo" control)

let get_t2mi_seq control seconds =
  get_result t2mi_seq_of_yojson (Printf.sprintf "/api/board/%d/t2mi_seq/%d" control seconds)

let get_structs control =
  get_result ts_structs_of_yojson (Printf.sprintf "/api/board/%d/structs" control)

let get_bitrates control =
  get_result ts_structs_of_yojson (Printf.sprintf "/api/board/%d/bitrates" control)

let get_section (req:section_request) control =
  post_result ~contents:(section_request_to_yojson req)
              ~from_err:section_error_of_yojson
              section_of_yojson
              (Printf.sprintf "/api/board/%d/get_section" control)

let get_incoming_streams control =
  get_result Common.Stream.t_list_of_yojson (Printf.sprintf "/api/board/%d/incoming_streams" control)

let get_config_ws control =
  WS.get (Printf.sprintf "api/board/%d/config_ws" control) config_of_yojson

let get_status_ws control =
  WS.get (Printf.sprintf "api/board/%d/status_ws" control) status_of_yojson

let get_t2mi_errors_ws control =
  WS.get (Printf.sprintf "api/board/%d/t2mi_errors_ws" control) t2mi_errors_of_yojson

let get_board_errors_ws control =
  WS.get (Printf.sprintf "api/board/%d/board_errors_ws" control) board_error_of_yojson

let get_bitrates_ws control =
  WS.get (Printf.sprintf "api/board/%d/bitrate_ws" control) ts_structs_of_yojson

let get_structs_ws control =
  WS.get (Printf.sprintf "api/board/%d/structs_ws" control) ts_structs_of_yojson

let get_t2mi_info_ws control =
  WS.get (Printf.sprintf "api/board/%d/t2mi_info_ws" control) t2mi_info_of_yojson

let get_jitter_ws control =
  WS.get (Printf.sprintf "api/board/%d/jitter_ws" control) jitter_of_yojson

let get_incoming_streams_ws control =
  WS.get (Printf.sprintf "api/board/%d/incoming_streams" control) Common.Stream.t_list_of_yojson

module Streams = struct

  type ts_structs_response =
    { structs  : ts_structs
    ; has_more : bool
    }

  let get_streams () : (Common.Stream.t list,'a) rsp =

    Lwt_result.fail (`Code 404)

  let get_ts_struct () = ()

  let get_ts_structs () = ()

end

(* module Errors = struct
 * 
 *   module Real_time = struct
 * 
 *     let get_ts_errors_ws control =
 *       WS.get (Printf.sprintf "api/board/%d/ts_errors_ws" control) ts_errors_of_yojson
 * 
 *   end
 * 
 *   module Archive = struct
 * 
 *     open Common.Time
 *     open Board_types.Errors_api
 * 
 *     type body =
 *       { priority : priority option
 *       ; errors   : error_id list option
 *       ; period   : Interval.Seconds.t
 *       ; stream   : Common.Stream.t
 *       } [@@deriving yojson]
 * 
 *     type has_errors = bool [@@deriving of_yojson]
 * 
 *     let get_ts_errors ?priority ?errors period stream control : (ts_error list,'a) rsp =
 *       let contents = { priority; errors; period; stream } |> body_to_yojson in
 *       post_result ~contents ts_error_list_of_yojson (Printf.sprintf "/api/board/%d/ts_errors" control)
 * 
 *     let get_ts_errors_percentage ?priority ?errors period stream control : (percentage,'a) rsp =
 *       let contents = { priority; errors; period; stream } |> body_to_yojson in
 *       post_result ~contents percentage_of_yojson (Printf.sprintf "/api/board/%d/ts_errors_percentage" control)
 * 
 *     let has_ts_errors ?priority ?errors period stream control : (has_errors,'a) rsp =
 *       let contents = { priority; errors; period; stream } |> body_to_yojson in
 *       post_result ~contents has_errors_of_yojson (Printf.sprintf "/api/board/%d/has_ts_errors" control)
 * 
 *   end
 * 
 * end *)
