open Containers
open Board_types
open Structure_types
open Api_js.Requests.Json_request
open Lwt.Infix

include Boards_js.Requests

type ('a,'b) rsp = ('a,'b Api_js.Requests.err) Lwt_result.t

let to_unit = fun _ -> Ok ()

let get_t2mi_seq control seconds =
  get_result t2mi_packets_of_yojson (Printf.sprintf "/api/board/%d/t2mi_seq/%d" control seconds)

let get_si_psi_section (req:section_request) control =
  post_result ~contents:(section_request_to_yojson req)
              ~from_err:section_error_of_yojson
              section_of_yojson
              (Printf.sprintf "/api/board/%d/get_section" control)

module Board = struct

  (** Resets the board **)
  let post_reset control () =
    post_result to_unit (Printf.sprintf "/api/board/%d/reset" control)

  (** Sets board input to listen **)
  let post_input control inp =
    post_result ~contents:(input_to_yojson inp) to_unit (Printf.sprintf "/api/board/%d/input" control)

  (** Sets T2-MI analysis settings **)
  let post_t2mi_mode control mode =
    let path = Printf.sprintf "/api/board/%d/t2mi_mode" control in
    post_result ~contents:(t2mi_mode_request_to_yojson mode) to_unit path

  (** Sets jitter measurements settings **)
  let post_jitter_mode control mode =
    let path = Printf.sprintf "/api/board/%d/jitter_mode" control in
    post_result ~contents:(jitter_mode_request_to_yojson mode) to_unit path

  module Real_time = struct

    (** Returns board description if already available **)
    let get_devinfo control =
      get_result devinfo_response_of_yojson (Printf.sprintf "/api/board/%d/devinfo" control)

    (** Event is raised when board errors occur **)
    let get_board_errors_ws control =
      WS.get (Printf.sprintf "api/board/%d/board_errors_ws" control) board_error_of_yojson

    (** Returns current overall board configuration **)
    let get_config control =
      get_result config_of_yojson (Printf.sprintf "/api/board/%d/config" control)

    (** Event is raised when overall board configuration changes **)
    let get_config_ws control =
      WS.get (Printf.sprintf "api/board/%d/config_ws" control) config_of_yojson

    (** Event is raised when board status changes **)
    let get_status_ws control =
      WS.get (Printf.sprintf "api/board/%d/status_ws" control) Board.status_of_yojson

  end

  module Archive = struct

    open Common.Time

    let get_board_errors (period:Interval.t) control =
      let f = int_of_float @@ to_float_s @@ Interval.from period in
      let t = int_of_float @@ to_float_s @@ Interval.till period in
      let path = Printf.sprintf "/api/board/%d/board_errors/%d/%d" control f t in
      get_result board_errors_of_yojson path

    let get_statuses (period:Interval.t) control =
      let f = int_of_float @@ to_float_s @@ Interval.from period in
      let t = int_of_float @@ to_float_s @@ Interval.till period in
      let path = Printf.sprintf "/api/board/%d/statuses/%d/%d" control f t in
      get_result Board.statuses_of_yojson path

  end

end

module Streams = struct

  module Real_time = struct

    (* Stream availability *)

    (** Returns currently available incoming TS **)
    let get_incoming_streams control =
      get_result Common.Stream.t_list_of_yojson (Printf.sprintf "/api/board/%d/incoming_streams" control)

    (** Event is raised when list of incoming TS changes **)
    let get_incoming_streams_ws control =
      WS.get (Printf.sprintf "api/board/%d/incoming_streams" control) Common.Stream.t_list_of_yojson

    (** Event is raised when TS is found or lost **)
    let get_ts_states_ws ?(stream:Common.Stream.id option) control =
      let base = Printf.sprintf "api/board/%d/ts_state_ws" control in
      let path = match stream with
        | Some x -> let id = Common.Stream.id_to_int32 x in Printf.sprintf "%s/%ld" base id
        | None   -> base
      in WS.get path ts_states_of_yojson

    (** Event is raised when t2mi stream is found or lost **)
    let get_t2mi_states_ws (stream:int option) control =
      let base = Printf.sprintf "api/board/%d/t2mi_stream_ws" control in
      let path = match stream with
        | Some x -> Printf.sprintf "%s/%d" base x
        | None   -> base
      in WS.get path t2mi_states_of_yojson

    (* Structs *)

    (** Returns current structure of requested TS **)
    let get_ts_struct (sid:Common.Stream.id) control =
      let id = Common.Stream.id_to_int32 sid in
      get_result ts_struct_of_yojson (Printf.sprintf "/api/board/%d/ts_struct/%ld" control id)

    (** Returns current structures of all available TSs **)
    let get_ts_structs control =
      get_result ts_structs_of_yojson (Printf.sprintf "/api/board/%d/ts_struct" control)

    (** Event is raised when structure of corresponding TS changes **)
    let get_ts_struct_ws (sid:Common.Stream.id) control =
      let id = Common.Stream.id_to_int32 sid in
      WS.get (Printf.sprintf "api/board/%d/ts_struct_ws/%ld" control id) ts_struct_of_yojson

    (** Event is raised when structures of all available TSs change **)
    let get_ts_structs_ws control =
      WS.get (Printf.sprintf "api/board/%d/ts_struct_ws" control) ts_structs_of_yojson

    (** Returns current structure of requested TS **)
    let get_t2mi_info ?(stream:int option) control =
      let base = Printf.sprintf "/api/board/%d/t2mi_info" control in
      let path = match stream with
        | Some x -> Printf.sprintf "%s/%d" base x
        | None   -> base
      in get_result t2mi_info_response_of_yojson path

    (** Event is raised when T2-MI info changes **)
    let get_t2mi_info_ws ?(stream:int option) control =
      let base = Printf.sprintf "api/board/%d/t2mi_info_ws" control in
      let path = match stream with
        | Some x -> Printf.sprintf "%s/%d" base x
        | None   -> base
      in WS.get path t2mi_info_response_of_yojson

    (* Bitrates *)

    (** Returns current bitrates of requested stream **)
    let get_ts_bitrate (sid:Common.Stream.id) control =
      let id = Common.Stream.id_to_int32 sid in
      get_result ts_structs_of_yojson (Printf.sprintf "/api/board/%d/ts_bitrate/%ld" control id)

    (** Returns current bitrates of all available ts streams **)
    let get_ts_bitrates control =
      get_result ts_structs_of_yojson (Printf.sprintf "/api/board/%d/ts_bitrate" control)

    (** Events is raised when bitrates of corresponding streams changes **)
    let get_ts_bitrate_ws (sid:Common.Stream.id) control =
      let id = Common.Stream.id_to_int32 sid in
      get_result ts_structs_of_yojson (Printf.sprintf "/api/board/%d/ts_bitrate_ws/%ld" control id)

    (** Event is raised when bitrates of all available ts streams change **)
    let get_ts_bitrates_ws control =
      WS.get (Printf.sprintf "api/board/%d/ts_bitrate_ws" control) ts_structs_of_yojson

  end

end

module Jitter = struct

  module Real_time = struct

    let get_jitter_ws control =
      WS.get (Printf.sprintf "api/board/%d/jitter_ws" control) jitter_measures_of_yojson

  end

  module Archive = struct

  end

end

module Errors = struct

  (* MPEG-2 TS and T2-MI errors according mostly to ETSI TR 101 290 *)

  module Real_time = struct

    (** Event is raised when errors occur in a stream(s) **)
    let get_ts_errors_ws ?(stream:Common.Stream.id option) control =
      let base = Printf.sprintf "api/board/%d/ts_errors_ws" control in
      let path = match stream with
        | Some sid -> let id = Common.Stream.id_to_int32 sid in Printf.sprintf "%s/%ld" base id
        | None     -> base
      in WS.get path ts_errors_of_yojson

    (** Event is raised when errors occur in any of available t2mi streams **)
    let get_t2mi_errors_ws ?(stream:int option) control =
      let base = (Printf.sprintf "api/board/%d/t2mi_errors_ws" control) in
      let path = match stream with
        | Some sid -> Printf.sprintf "%s/%d" base sid
        | None     -> base
      in WS.get path t2mi_errors_of_yojson

  end

  module Archive = struct

    open Common.Time
    open Board_types.Errors.Api

    type has_errors = bool [@@deriving of_yojson]

    (* MPEG-2 TS, ISO 13818-2 *)

    (** Returns a list of ts errors with additinal time intervals for requested time period **)
    let get_ts_errors ?priority ?errors ?stream period control : (ts_errors_rsp_archive,'a) rsp =
      let contents = { priority; errors; period; stream } |> ts_errors_req_archive_to_yojson in
      let path     = Printf.sprintf "/api/board/%d/ts_errors_archive" control in
      post_result ~contents ts_errors_rsp_archive_of_yojson path

    (** Returns segmentation of requested time period for TS(s) **)
    let get_ts_segmentation ?priority ?errors ?stream period control : (segmentation,'a) rsp =
      let contents = { priority; errors; period; stream } |> ts_errors_req_archive_to_yojson in
      let path     = Printf.sprintf "/api/board/%d/ts_segmentation" control in
      post_result ~contents segmentation_of_yojson path

    (** Returns flag indicating presence of errors for requested time period **)
    let has_ts_errors ?priority ?errors ?stream period control : (has_errors,'a) rsp =
      let contents = { priority; errors; period; stream } |> ts_errors_req_archive_to_yojson in
      let path     = Printf.sprintf "/api/board/%d/has_ts_errors" control in
      post_result ~contents has_errors_of_yojson path

    (* T2-MI, ETSI TS 102773 *)

    (** Returns a list of ts errors with additinal time intervals for requested time period **)
    let get_t2mi_errors ?errors ?stream period control : (ts_errors_rsp_archive,'a) rsp =
      let contents = { errors; period; stream } |> t2mi_errors_req_archive_to_yojson in
      let path     = Printf.sprintf "/api/board/%d/t2mi_errors_archive" control in
      post_result ~contents ts_errors_rsp_archive_of_yojson path

    (** Returns segmentation of requested time period for T2-MI stream(s) **)
    let get_t2mi_segmentation ?errors ?stream period control : (segmentation,'a) rsp =
      let contents = { errors; period; stream } |> t2mi_errors_req_archive_to_yojson in
      let path     = Printf.sprintf "/api/board/%d/t2mi_segmentation" control in
      post_result ~contents segmentation_of_yojson path

    (** Returns flag indicating presence of errors for requested time period **)
    let has_t2mi_errors ?errors ?stream period control : (has_errors,'a) rsp =
      let contents = { errors; period; stream } |> t2mi_errors_req_archive_to_yojson in
      let path     = Printf.sprintf "/api/board/%d/has_t2mi_errors" control in
      post_result ~contents has_errors_of_yojson path

  end

end
