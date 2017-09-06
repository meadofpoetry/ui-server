[@@@ocaml.warning "-32"]

[%%cenum
 type einput =
   | SPI
   | ASI [@@uint8_t]]

[%%cstruct
 type common_header =
   { prefix   : uint16_t
   ; msg_code : uint16_t
   } [@@little_endian]]

[%%cstruct
 type complex_req_header =
   { client_id  : uint16_t
   ; length     : uint16_t
   ; request_id : uint16_t
   } [@@little_endian]]

[%%cstruct
 type board_info =
   { board_type    : uint8_t
   ; board_version : uint8_t
   ; rfu           : uint16_t
   } [@@little_endian]]

[%%cstruct
 type board_mode =
   { mode           : uint8_t
   ; rfu            : uint8_t
   ; t2mi_pid       : uint16_t
   ; t2mi_stream_id : uint32_t
   } [@@little_endian]]

[%%cstruct
 type req_get_t2mi_frame_seq =
   { time : uint16_t
   } [@@little_endian]]

[%%cstruct
 type req_get_ts_struct =
   { stream_id : uint32_t
   } [@@little_endian]]

[%%cstruct
 type req_get_t2mi_info =
   { rfu : uint16_t
   ; stream_id : uint32_t
   } [@@little_endian]]

[%%cstruct
 type status =
   { rfu              : uint32_t
   ; ts_num           : uint8_t
   ; streams_ver      : uint8_t
   ; load             : uint8_t
   ; structs_ver      : uint8_t
   ; bitrate          : uint32_t
   ; mode             : uint8_t
   ; rfu_1            : uint8_t
   ; t2mi_pid         : uint16_t
   ; t2mi_stream_id   : uint32_t
   ; rfu_2            : uint16_t
   ; flags            : uint8_t
   ; services_num     : uint8_t
   ; t2mi_sync        : uint8_t
   ; rfu_3            : uint8_t [@len 7]
   ; flags_2          : uint16_t
   ; status_ver       : uint16_t
   ; t2mi_info_ver    : uint32_t
   ; rfu_4            : uint32_t [@len 7]
   ; stream_ver       : uint8_t [@len 50]
   ; rfu_5            : uint8_t [@len 50]
   ; jitter_stream_id : uint32_t
   ; jitter_pid       : uint16_t
   ; rfu_6            : uint16_t
   ; total_packet_cnt : uint32_t
   ; ts_absent        : uint16_t [@len 8]
   ; ts_not_verified  : uint16_t [@len 8]
   ; rfu_7            : uint16_t [@len 146]
   } [@@little_endian]]

[%%cstruct
 type ts_error =
   { rfu      : uint16_t
   ; count    : uint16_t
   ; err_code : uint8_t
   ; err_ext  : uint8_t
   ; pid      : uint16_t
   ; packet   : uint32_t
   ; param_1  : uint32_t
   ; param_2  : uint32_t
   } [@@little_endian]]

[%%cstruct
 type ts_errors =
   { length    : uint16_t
   ; rfu_1     : uint16_t
   ; stream_id : uint32_t
   ; rfu_2     : uint16_t
   ; count     : uint16_t
   } [@@little_endian]]

[%%cstruct
 type t2mi_error =
   { index : uint16_t
   ; data  : uint16_t
   } [@@little_endian]]

[%%cstruct
 type t2mi_errors =
   { length    : uint16_t
   ; rfu_1     : uint16_t
   ; stream_id : uint32_t
   ; pid       : uint16_t
   ; sync      : uint8_t
   ; rfu_2     : uint8_t
   ; err_flags : uint16_t
   ; rfu_3     : uint8_t [@len 5]
   ; count     : uint8_t
   } [@@little_endian]]

[@@@ocaml.warning "+32"]

open Common.Board.Qos

(* ------------------- Misc ------------------- *)

let prefix = 0x55AA

let io = fun x -> Lwt_io.printf "%s\n" x |> ignore

let to_common_header ~msg_code () =
  let hdr = Cbuffer.create sizeof_common_header in
  let () = set_common_header_prefix hdr prefix in
  let () = set_common_header_msg_code hdr msg_code in
  hdr

let to_complex_req_header ?(client_id=0) ?(request_id=0) ~msg_code ~length () =
  let hdr = to_common_header ~msg_code () in
  let complex_hdr = Cbuffer.create sizeof_complex_req_header in
  let () = set_complex_req_header_client_id complex_hdr client_id in
  let () = set_complex_req_header_length complex_hdr length in
  let () = set_complex_req_header_request_id complex_hdr request_id in
  Cbuffer.append hdr complex_hdr

let to_simple_req ~msg_code ~body () =
  let hdr = to_common_header ~msg_code () in
  Cbuffer.append hdr body

let to_complex_req ?client_id ?request_id ~msg_code ~body () =
  let length = (Cbuffer.len body / 2) + 1 in
  let hdr = to_complex_req_header ?client_id ?request_id ~msg_code ~length () in
  Cbuffer.append hdr body

(* ------------------- Requests/responses ------------------- *)

let of_input : input -> einput = function
  | SPI -> SPI | ASI -> ASI
let to_input : einput -> input = function
  | SPI -> SPI | ASI -> ASI

(* Get board info *)

let to_req_get_board_info = to_common_header ~msg_code:0x0080

let of_rsp_get_board_info msg =
  { typ = get_board_info_board_type msg
  ; ver = get_board_info_board_version msg
  }

(* Get board mode *)

let to_req_get_board_mode = to_common_header ~msg_code:0x0081

let to_mode mode t2mi_pid t2mi_stream_id =
  { input = to_input @@ CCOpt.get_exn @@ int_to_einput (mode land 1)
  ; t2mi = Some { enabled   = if (mode land 4) > 0 then true else false
                ; pid       = t2mi_pid
                ; stream_id = Stream.of_int32 t2mi_stream_id
                }
  }

let of_rsp_get_board_mode msg =
  let mode = get_board_mode_mode msg in
  to_mode mode
          (get_board_mode_t2mi_pid msg)
          (get_board_mode_t2mi_stream_id msg)

(* Set board mode *)

let to_req_set_board_mode ~mode =
  let t2mi_mode = CCOpt.get_or ~default:{ enabled   = false
                                        ; pid       = 0
                                        ; stream_id = Stream.src_single
                                        }
                               mode.t2mi in
  let body = Cbuffer.create sizeof_board_mode in
  let ()   = einput_to_int @@ of_input mode.input
             |> (lor) (if t2mi_mode.enabled then 4 else 0)
             |> set_board_mode_mode body in
  let ()   = set_board_mode_t2mi_pid body t2mi_mode.pid in
  let ()   = set_board_mode_t2mi_stream_id body (Stream.to_int32 t2mi_mode.stream_id) in
  to_simple_req ~msg_code:0x0082 ~body

(* Get board errors *)

let to_req_get_board_errors ?client_id ?request_id =
  to_complex_req ?client_id ?request_id ~msg_code:0x0110 ~body:(Cbuffer.create 0)

(* Get section *)

let to_req_get_section ?client_id ?request_id () = ()

(* Get simple table *)

let to_req_get_simple_table ?client_id ?request_id () = ()

(* Get t2mi frames sequence *)

let to_req_get_t2mi_frame_seq ?client_id ?request_id ~seconds =
  let body = Cbuffer.create sizeof_req_get_t2mi_frame_seq in
  let ()   = set_req_get_t2mi_frame_seq_time body seconds in
  to_complex_req ?client_id ?request_id ~msg_code:0x0306 ~body

(* Get struct *)

let to_req_get_ts_struct ?client_id ?request_id ~stream =
  let body = Cbuffer.create sizeof_req_get_ts_struct in
  let ()   = set_req_get_ts_struct_stream_id body @@ Stream.to_int32 stream in
  to_complex_req ?client_id ?request_id ~msg_code:0x0309 ~body

let to_req_get_ts_structs ?client_id ?request_id =
  let stream = (Stream.of_int32 Unsigned.(UInt32.max_int |> UInt32.to_int32)) in
  to_req_get_ts_struct ?client_id ?request_id ~stream

(* Get bitrate *)

let to_req_get_ts_bitrate ?client_id ?request_id =
  to_complex_req ?client_id ?request_id ~msg_code:0x030A ~body:(Cbuffer.create 0)

(* Get t2mi info *)

let to_req_get_t2mi_info ?client_id ?request_id ~stream =
  let body = Cbuffer.create sizeof_req_get_t2mi_info in
  let ()   = set_req_get_t2mi_info_stream_id body @@ Stream.to_int32 stream in
  to_complex_req ?client_id ?request_id ~msg_code:0x030B ~body

(* Get streams list *)

let to_req_get_streams_list ?client_id ?request_id =
  to_complex_req ?client_id ?request_id ~msg_code:0x030C ~body:(Cbuffer.create 0)

(* ------------------- Events ------------------- *)

(* Status *)

let of_status msg =
  let flags      = get_status_flags msg in
  let has_stream = if (flags land 0x80) > 0 then false else true in
  let has_sync   = if (flags land 0x04) > 0 then false else true in
  let packet_fmt = if (flags land 0x08) > 0 then Ts192
                   else if (flags land 0x10) > 0 then Ts204
                   else Ts188 in
  let mode       = get_status_mode msg in
  { ts_num       = get_status_ts_num msg
  ; load         = (get_status_load msg) * 100 / 255
  ; bitrate      = get_status_bitrate msg
  ; mode         = to_mode mode (get_status_t2mi_pid msg) (get_status_t2mi_stream_id msg)
  ; has_stream
  ; has_sync
  ; packet_fmt
  ; services_num = get_status_services_num msg
  }

(* Ts errors *)

let of_ts_error msg =
  let pid'      = get_ts_error_pid msg in
  let pid       = pid' land 0x1FFF in
  let multi_pid = if (pid' land 0x8000) > 0 then true else false in
  { count     = get_ts_error_count msg
  ; err_code  = get_ts_error_err_code msg
  ; err_ext   = get_ts_error_err_ext msg
  ; multi_pid
  ; pid
  ; packet    = get_ts_error_packet msg
  ; param_1   = get_ts_error_param_1 msg
  ; param_2   = get_ts_error_param_2 msg
  }

let of_ts_errors msg =
  let common,errors = Cbuffer.split msg sizeof_ts_errors in
  let number        = get_ts_errors_count common in
  if number * sizeof_ts_error = Cbuffer.len errors
  then { stream_id = Stream.of_int32 (get_ts_errors_stream_id common)
       ; errors    = List.fold_left (fun acc x -> if sizeof_ts_error = Cbuffer.len x
                                                  then (of_ts_error x) :: acc
                                                  else acc)
                                    []
                                    (Cbuffer.split_size sizeof_ts_error errors)
       }
  else failwith "bad ts errors payload"

(* T2-MI errors *)

let of_t2mi_error msg =
  let _index   = get_t2mi_error_index msg in
  let _data    = get_t2mi_error_data msg in
  let cnt_flag = if (_index land 4) = 0 then true else false in
  { count          = if cnt_flag then Some _data else None
  ; err_code       = _index lsr 4
  ; t2mi_stream_id = _index land 3
  ; param          = if cnt_flag then None else Some _data
  }

let of_t2mi_errors msg =
  let common,errors = Cbuffer.split msg sizeof_t2mi_errors in
  let number        = get_t2mi_errors_count common in
  if number * sizeof_t2mi_error = Cbuffer.len errors
  then { stream_id     = Stream.of_int32 (get_t2mi_errors_stream_id common)
       ; t2mi_pid      = get_t2mi_errors_pid common
       ; sync          = []
       ; parser_errors = get_t2mi_errors_err_flags common
       ; errors        = List.fold_left (fun acc x -> if sizeof_t2mi_error = Cbuffer.len x
                                                      then (of_t2mi_error x) :: acc
                                                      else acc)
                                        []
                                        (Cbuffer.split_size sizeof_t2mi_error errors)
       }

(* ------------------- Board protocol implementation ------------------- *)

type init_conf = unit

type resp = Board_info of info
          | Board_mode of mode
          | Status     of status
          | Ts_errors  of ts_errors

type req = Set_board_mode of mode
         | Get_board_info
         | Get_board_mode

let (init:req) = Set_board_mode { input = SPI
                                ; t2mi  = Some { enabled = false
                                               ; pid     = 4096
                                               ; stream_id = Stream.src_single
                                               }
                                }

let probes _ = []

let period = 5

let to_init_conf : resp -> init_conf option = fun _ -> Some ()

let make_req (s,_) =
  match s with
  | _ -> Error "unknown request"

let to_yojson : resp -> Yojson.Safe.json = function
  | Board_info x -> info_to_yojson x
  | Board_mode x -> mode_to_yojson x
  | Status x     -> status_to_yojson x
  | Ts_errors x  -> ts_errors_to_yojson x

let (serialize : req -> Board_meta.req_typ * Cbuffer.t) = function
  | Set_board_mode mode -> `Need_response, to_req_set_board_mode ~mode
  | Get_board_info      -> `Need_response, to_req_get_board_info
  | Get_board_mode      -> `Need_response, to_req_get_board_mode

(* Deserialization *)

type err = Bad_prefix of int
         | Bad_length of int
         | Bad_msg_code of int
         | No_prefix_after_msg
         | Insufficient_payload of Cbuffer.t
         | Unknown_err of string

let string_of_err = function
  | Bad_prefix x            -> "incorrect start tag: "    ^ (string_of_int x)
  | Bad_length x            -> "incorrect length: "       ^ (string_of_int x)
  | Bad_msg_code x          -> "incorrect code: "         ^ (string_of_int x)
  | No_prefix_after_msg     -> "no prefix found after message payload"
  | Insufficient_payload _  -> "insufficient payload"
  | Unknown_err s           -> s

let check_prefix msg =
  let prefix' = get_common_header_prefix msg in
  if prefix != prefix' then Error (Bad_prefix prefix') else Ok msg

let check_msg_code msg =
  let code = get_common_header_msg_code msg in
  match code lsr 8 with
  | 0x01 | 0x02 | 0x03 | 0x04 | 0x05 | 0x09 | 0xFD | 0xFF -> Ok msg
  | _ -> Error (Bad_msg_code code)

let check_length_and_crop msg =
  let hdr,rest = Cbuffer.split msg sizeof_common_header in
  let code = get_common_header_msg_code hdr in
  try
    let length = (match code lsr 8 with
                  | 0x01 -> 4                                         (* board info*)
                  | 0x02 -> 8                                         (* board mode *)
                  | 0x03 -> 504                                       (* status *)
                  | 0x04 -> ((get_ts_errors_length rest) * 2) + 2     (* ts errors *)
                  | 0x05 -> 0                                         (* t2mi errors *)
                  | 0x09 -> get_complex_req_header_length rest        (* complex msg *)
                  | 0xFD -> 4                                         (* end of errors *)
                  | 0xFF -> 0                                         (* end of transmission *)
                  | _    -> failwith "unknown message code") in
    if length <= ((256 * 2) - sizeof_common_header) (* max payload length *)
    then let body,next_data = Cbuffer.split rest length in
         let valid_msg = Cbuffer.append hdr body in
         if Cbuffer.len next_data < sizeof_common_header
         then Ok valid_msg
         else (match check_prefix next_data with
               | Ok _    -> Ok valid_msg
               | Error _ -> Error No_prefix_after_msg)
    else Error (Bad_length length)
  with
  | Invalid_argument _ -> Error (Insufficient_payload msg)
  | Failure _          -> Error (Bad_msg_code code)
  | e                  -> Error (Unknown_err (Printexc.to_string e))

let get_msg msg =
  try
    CCResult.(check_prefix msg
              >>= check_msg_code
              >>= check_length_and_crop)
  with e -> Error (Unknown_err (Printexc.to_string e))

let deserialize buf =
  io (Printf.sprintf "in deserialize %d" @@ Cbuffer.len buf);
  let parse_msg = fun msg ->
    try
      let code = get_common_header_msg_code msg lsr 8 in
      let _,body = Cbuffer.split msg sizeof_common_header in
      (match code with
       | 0x01 -> Some (Board_info (of_rsp_get_board_info body))
       | 0x02 -> Some (Board_mode (of_rsp_get_board_mode body))
       | 0x03 -> Some (Status (of_status body))
       | 0x04 -> Some (Ts_errors (of_ts_errors body))
       | _ -> None)
    with e -> io @@ Printexc.to_string e; None in
  let rec f acc b =
    if Cbuffer.len b >= sizeof_common_header
    then (match get_msg b with
          | Ok msg    -> let _,res = Cbuffer.split b (Cbuffer.len msg) in
                         io (Printf.sprintf "Got a message! %d" (Cbuffer.len msg));
                         parse_msg msg |> (function
                                           | Some x -> f (x::acc) res
                                           | None   -> f acc res)
          | Error e -> (match e with
                        | Insufficient_payload x -> io "Insufficient_payload"; List.rev acc, x
                        | _ -> io (string_of_err e); Cbuffer.split b 1 |> fun (_,x) -> f acc x))
    else List.rev acc, b in
  let msgs, res = f [] buf in
  (msgs, if Cbuffer.len res > 0 then Some res else None)

let is_response req resp =
  match (req,resp) with
  | Get_board_info, Board_info _ -> Some resp
  | Get_board_mode, Board_mode _ -> Some resp
  | Set_board_mode _, _          -> Some resp
  | _                            -> None

let is_free : resp -> resp option = function
  | Status _ as x    -> Some x
  | Ts_errors _ as x -> Some x
  | _                -> None
