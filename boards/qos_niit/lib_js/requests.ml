open Containers
open Board_types
open Api_js.Requests.Json
open Api_utils
open Lwt.Infix
open Common

include Boards_js.Requests

type ('a,'b) rsp = ('a,'b Api_js.Requests.err) Lwt_result.t

let to_unit = fun _ -> Ok ()

(* (\** Returns T2-MI packet sequence for requested time span **\)
 * let get_t2mi_seq control seconds =
 *   get_result t2mi_packets_of_yojson (Printf.sprintf "/api/board/%d/t2mi_seq/%d" control seconds)
 * 
 * (\** Returns SI/PSI section if found in the requested stream **\)
 * let get_si_psi_section (req:section_request) control =
 *   post_result ~contents:(section_request_to_yojson req)
 *               ~from_err:section_error_of_yojson
 *               section_of_yojson
 *               (Printf.sprintf "/api/board/%d/get_section" control) *)

module Device = Requests_device


module Streams = struct

  open Board_types.Streams

  module RT = struct

    (* Stream availability *)

    (** Returns currently available incoming TS **)
    let get_input_streams control =
      let path = Path.(make_full "input_streams" control |> to_string) in
      get_result Common.Stream.t_list_of_yojson path

    (** Event is raised when list of incoming TS changes **)
    let get_input_streams_ws control =
      let path = Path.(make_full) in
      WS.get path Common.Stream.t_list_of_yojson

    (** Returns currently analyzed TS (incoming and self-generated) **)
    let get_streams control =
      let path = Printf.sprintf "/api/board/%d/streams" control in
      get_result Common.Stream.t_list_of_yojson path

    (** Event is raised when list of analyzed TS changes **)
    let get_streams_ws control =
      let path = Printf.sprintf "api/board/%d/streams_ws" control in
      WS.get path Common.Stream.t_list_of_yojson

    (** Event is raised when requested TS is found or lost **)
    let get_ts_state_ws (stream:Stream.id) control =
      let id   = Stream.id_to_int32 stream in
      let path = Printf.sprintf "api/board/%d/ts_state_ws/%ld" control id in
      WS.get path TS.state_of_yojson

    (** Event is raised when any TS is found or lost **)
    let get_ts_states_ws control =
      let path = Printf.sprintf "api/board/%d/ts_state_ws" control in
      WS.get path TS.states_of_yojson

    (** Event is raised when requested T2-MI stream is found or lost **)
    let get_t2mi_state (stream:int) control =
      let path = Printf.sprintf "api/board/%d/t2mi_stream_ws/%d" control stream in
      WS.get path Streams.T2MI.state_of_yojson

    (** Event is raised when any T2-MI stream is found or lost **)
    let get_t2mi_states_ws control =
      let path = Printf.sprintf "api/board/%d/t2mi_stream_ws" control in
      WS.get path Streams.T2MI.states_of_yojson

    (* Structs *)

    (** Returns current structure of requested TS **)
    let get_ts_struct (sid:Stream.id) control =
      let id   = Common.Stream.id_to_int32 sid in
      let path = Printf.sprintf "/api/board/%d/ts_struct/%ld" control id in
      get_result TS.structure_response_of_yojson path

    (** Returns current structures of all available TSs **)
    let get_ts_structs control =
      let path = Printf.sprintf "/api/board/%d/ts_struct" control in
      get_result TS.structures_of_yojson path

    (** Event is raised when structure of corresponding TS changes **)
    let get_ts_struct_ws (sid:Stream.id) control =
      let id   = Common.Stream.id_to_int32 sid in
      let path = Printf.sprintf "api/board/%d/ts_struct_ws/%ld" control id in
      WS.get path TS.structure_of_yojson

    (** Event is raised when structures of all available TSs change **)
    let get_ts_structs_ws control =
      let path = Printf.sprintf "api/board/%d/ts_struct_ws" control in
      WS.get path TS.structures_of_yojson

    (** Returns current structure of requested T2-MI stream if available **)
    let get_t2mi_info_for_stream (stream:int) control =
      let path = Printf.sprintf "/api/board/%d/t2mi_info/%d" control stream in
      get_result T2MI.structure_response_of_yojson path

    (** Returns current structures of T2-MI streams **)
    let get_t2mi_info control =
      let path = Printf.sprintf "/api/board/%d/t2mi_info" control in
      get_result T2MI.structures_of_yojson path

    (** Event is raised when T2-MI info of corresponding stream changes **)
    let get_t2mi_info_for_stream_ws (stream:int) control =
      let path = Printf.sprintf "api/board/%d/streams/ts/structure_ws/%d" control stream in
      WS.get path T2MI.structure_of_yojson

    (** Event is raised when T2-MI info changes **)
    let get_t2mi_info_ws control =
      let path = Printf.sprintf "api/board/%d/t2mi_info_ws" control in
      WS.get path T2MI.structures_of_yojson

    (* Bitrates *)

    (** Returns current bitrates of requested stream **)
    let get_bitrate (sid:Common.Stream.id) control =
      let id = Common.Stream.id_to_int32 sid in
      get_result TS.structure_response_of_yojson (Printf.sprintf "/api/board/%d/bitrate/%ld" control id)

    (** Returns current bitrates of all available ts streams **)
    let get_bitrates control =
      get_result TS.structures_of_yojson (Printf.sprintf "/api/board/%d/bitrate" control)

    (** Events is raised when bitrates of corresponding streams changes **)
    let get_bitrate_ws (sid:Common.Stream.id) control =
      let id   = Common.Stream.id_to_int32 sid in
      let path = Printf.sprintf "/api/board/%d/bitrate_ws/%ld" control id in
      get_result TS.structure_of_yojson path

    (** Event is raised when bitrates of all available ts streams change **)
    let get_bitrates_ws control =
      let path = Printf.sprintf "api/board/%d/bitrate_ws" control in
      WS.get path TS.structures_of_yojson

  end

  module AR = struct

  end

end

module Errors = struct

  (* MPEG-2 TS and T2-MI errors according mostly to ETSI TR 101 290 *)

  module RT = struct

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

  module AR = struct

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
