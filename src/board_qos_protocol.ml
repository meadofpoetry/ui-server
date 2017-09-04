[@@@ocaml.warning "-32"]

[%%cenum
 type input =
   | SPI
   | ASI [@@uint8_t]]

let input_to_yojson x = `String (input_to_string x)
let input_of_yojson = function
  | `String s -> (match string_to_input s with
                 | Some x -> Ok x
                 | None   -> Error ("input_of_yojson: unknown value " ^ s))
  | e         -> Error ("input_of_yojson: unknown value " ^ (Yojson.Safe.to_string e))

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

[@@@ocaml.warning "+32"]

let prefix = 0x55AA
let max_payload_len = (256 * 2) - sizeof_common_header

(* Helper functions *)

let io = fun x -> Lwt_io.printf "%s\n" x |> ignore

let mode_to_int input t2mi_enabled =
  (input_to_int input) lor (if t2mi_enabled then 4 else 0)

(* Message constructors *)

let to_common_header ~msg_code =
  let hdr = Cbuffer.create sizeof_common_header in
  let () = set_common_header_prefix hdr prefix in
  let () = set_common_header_msg_code hdr msg_code in
  hdr

let to_complex_req_header ?(client_id=0) ?(request_id=0) ~msg_code ~length =
  let hdr = to_common_header ~msg_code in
  let complex_hdr = Cbuffer.create sizeof_complex_req_header in
  let () = set_complex_req_header_client_id complex_hdr client_id in
  let () = set_complex_req_header_length complex_hdr length in
  let () = set_complex_req_header_request_id complex_hdr request_id in
  Cbuffer.append hdr complex_hdr

let to_simple_req ~msg_code ~body =
  let hdr = to_common_header ~msg_code in
  Cbuffer.append hdr body

let to_complex_req ?client_id ?request_id ~msg_code ~body =
  let length = (Cbuffer.len body / 2) + 1 in
  let hdr = to_complex_req_header ?client_id ?request_id ~msg_code ~length in
  Cbuffer.append hdr body

(* Message validation *)

type err = Bad_prefix of int
         | Bad_length of int
         | Bad_msg_code of int
         | Insufficient_payload of Cbuffer.t
         | Unknown_err of string

let string_of_err = function
  | Bad_prefix x            -> "incorrect start tag: "    ^ (string_of_int x)
  | Bad_length x            -> "incorrect length: "       ^ (string_of_int x)
  | Bad_msg_code x          -> "incorrect code: "         ^ (string_of_int x)
  | Insufficient_payload _  -> "insufficient payload"
  | Unknown_err s           -> s

let check_prefix msg =
  let prefix' = get_common_header_prefix msg in
  if prefix != prefix' then Error (Bad_prefix prefix') else Ok msg

let check_msg_code msg =
  let code = get_common_header_msg_code msg in
  match code lsr 8 with
  | 0x01 | 0x02 | 0x03 | 0x04 | 0x05 | 0x09 | 0xFF -> Ok msg
  | _ -> Error (Bad_msg_code code)

let check_length msg =
  let hdr,msg' = Cbuffer.split msg sizeof_common_header in
  let code = get_common_header_msg_code hdr in
  try
    let length = (match code lsr 8 with
                  | 0x01 -> 4
                  | 0x02 -> 8
                  | 0x03 -> 504
                  | 0x04 -> 0
                  | 0x05 -> 0
                  | 0x09 -> get_complex_req_header_length msg'
                  | 0xFF -> 0
                  | _    -> failwith "unknown message code") in
    if length <= max_payload_len
    then let body,_ = Cbuffer.split msg' length in
         Ok (Cbuffer.append hdr body)
    else Error (Bad_length length)
  with
  | Invalid_argument _ -> Error (Insufficient_payload msg)
  | Failure _          -> Error (Bad_msg_code code)
  | e                  -> Error (Unknown_err (Printexc.to_string e))

let check_msg msg =
  try
    CCResult.(check_prefix msg
              >>= check_msg_code
              >>= check_length)
  with e -> Error (Unknown_err (Printexc.to_string e))

(* Requests/responses *)

type t2mi_mode =
  { enabled   : bool
  ; pid       : int
  ; stream_id : int32
  } [@@deriving yojson]

type info =
  { typ : int
  ; ver : int
  } [@@deriving to_yojson]

type mode =
  { input : input
  ; t2mi  : t2mi_mode option
  } [@@deriving yojson]

type packet_fmt = | Ts188
                  | Ts204
                  | Ts192

let packet_fmt_to_yojson = function
  | Ts188 -> `String "Ts188"
  | Ts204 -> `String "Ts204"
  | Ts192 -> `String "Ts192"

type status =
  { ts_num       : int
  ; load         : int
  ; bitrate      : int32
  ; mode         : mode
  ; has_stream   : bool
  ; has_sync     : bool
  ; packet_fmt   : packet_fmt
  ; services_num : int
  } [@@deriving to_yojson]

(* Get board info *)

let to_req_get_board_info () = to_common_header ~msg_code:0x0080

let of_rsp_get_board_info msg =
  { typ = get_board_info_board_type msg
  ; ver = get_board_info_board_version msg
  }

(* Get board mode *)

let to_req_get_board_mode () = to_common_header ~msg_code:0x0081

let to_mode mode t2mi_pid t2mi_stream_id =
  { input = CCOpt.get_exn @@ int_to_input (mode land 1)
  ; t2mi = Some { enabled   = if (mode land 4) > 0 then true else false
                ; pid       = t2mi_pid
                ; stream_id = t2mi_stream_id
                }
  }

let of_rsp_get_board_mode msg =
  let mode = get_board_mode_mode msg in
  to_mode mode
          (get_board_mode_t2mi_pid msg)
          (get_board_mode_t2mi_stream_id msg)

(* Set board mode *)

let to_req_set_board_mode (mode:mode) =
  let t2mi_mode = CCOpt.get_or ~default:{ enabled   = false
                                        ; pid       = 0
                                        ; stream_id = Int32.of_int 0
                                        }
                               mode.t2mi in
  let body = Cbuffer.create sizeof_board_mode in
  let ()   = set_board_mode_mode body (mode_to_int mode.input t2mi_mode.enabled) in
  let ()   = set_board_mode_t2mi_pid body t2mi_mode.pid in
  let ()   = set_board_mode_t2mi_stream_id body t2mi_mode.stream_id in
  to_simple_req ~msg_code:0x0082 ~body
  |> fun x -> Cbuffer.hexdump x; x

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

type init_conf = unit

type resp = Board_info of info
          | Board_mode of mode
          | Status of status

type req = Set_board_mode of mode
         | Get_board_info
         | Get_board_mode

(* Board protocol implementation *)

let (init:req) = Set_board_mode { input = ASI
                                ; t2mi  = Some { enabled = true
                                               ; pid     = 4096
                                               ; stream_id = Int32.of_int 0
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
  (* | _ -> `String "dummy" *)

let (serialize : req -> Board_meta.req_typ * Cbuffer.t) = function
  | Set_board_mode mode -> `Need_response, to_req_set_board_mode mode
  | Get_board_info      -> `Need_response, to_req_get_board_info ()
  | Get_board_mode      -> `Need_response, to_req_get_board_mode ()
  (* | _                   -> `Need_response, Cbuffer.create 1 *)

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
       | 0xFF -> None
       | _ -> None)
    with _ -> None in
  let rec f acc b =
    if Cbuffer.len b >= sizeof_common_header
    then (match check_msg b with
          | Ok x    -> let _,res = Cbuffer.split b (Cbuffer.len x) in
                       io (Printf.sprintf "Got a message! %d" (Cbuffer.len x));
                       x
                       |> parse_msg
                       |> (function
                           | Some x -> f (x::acc) res
                           | None   -> f acc res)
          | Error e -> (match e with
                        | (Bad_prefix _ | Bad_length _ | Bad_msg_code _ | Unknown_err _)  ->
                           (* io (string_of_err x); *)
                           let _, res = Cbuffer.split b 1 in
                           f acc res
                        | Insufficient_payload x ->
                           io "Insufficient_payload";
                           List.rev acc, x))
    else (io (Printf.sprintf "Small buffer %d" @@ Cbuffer.len b); List.rev acc, b) in
  let msgs, res = f [] buf in
  (msgs, if Cbuffer.len res > 0 then Some res else None)

let is_response req resp =
  match (req,resp) with
  | Get_board_info, Board_info _ -> Some resp
  | Get_board_mode, Board_mode _ -> Some resp
  | Set_board_mode _, _          -> Some resp
  | _                            -> None

let is_free : resp -> resp option = function
  | Status _ as x -> Some x
  | _             -> None
