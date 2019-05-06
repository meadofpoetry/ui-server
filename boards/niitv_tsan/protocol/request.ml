open Application_types
open Board_niitv_tsan_types
open Message

type error =
  | Timeout
  | Queue_overflow
  | Not_responding
  | Invalid_length
  | Invalid_payload
  | Custom of string

let error_to_string = function
  | Timeout -> "timeout"
  | Queue_overflow -> "queue overflow"
  | Not_responding -> "not responding"
  | Invalid_length -> "invalid length"
  | Invalid_payload -> "invalid payload"
  | Custom s -> s

type req_tag =
  [ `Set_source_id
  | `Set_mode
  | `Set_jitter_mode
  | `Get_devinfo
  | `Get_mode
  ]

type rsp_tag =
  [ `Devinfo
  | `Mode
  | `Status
  | `Streams
  | `Ts_errors
  | `T2mi_errors
  | `Part
  | `End_of_errors
  | `End_of_transmission
  ]

type complex_tag =
  [ `Reset
  | `Deverr
  | `Section
  | `T2mi_seq
  | `Jitter
  | `Bitrate
  | `Structure
  | `T2mi_info
  ]

let req_tag_to_enum : req_tag -> int = function
  | `Get_devinfo -> 0x0080
  | `Get_mode -> 0x0081
  | `Set_mode -> 0x0082
  | `Set_source_id -> 0x0089
  | `Set_jitter_mode -> 0x0112

let rsp_tag_to_string : rsp_tag -> string = function
  | `Devinfo -> "devinfo"
  | `Mode -> "mode"
  | `Status -> "status"
  | `Streams -> "streams"
  | `Ts_errors -> "TS errors"
  | `T2mi_errors -> "T2-MI errors"
  | `Part -> "part"
  | `End_of_errors -> "end of errors"
  | `End_of_transmission -> "end of transmission"

let rsp_tag_to_enum : rsp_tag -> int = function
  | `Devinfo -> 0x01
  | `Mode -> 0x02
  | `Status -> 0x03
  | `Ts_errors -> 0x04
  | `T2mi_errors -> 0x05
  | `Part -> 0x09
  | `Streams -> 0x0B
  | `End_of_errors -> 0xFD
  | `End_of_transmission -> 0xFF

let rsp_tag_of_enum : int -> rsp_tag option = function
  | 0x01 -> Some `Devinfo
  | 0x02 -> Some `Mode
  | 0x03 -> Some `Status
  | 0x04 -> Some `Ts_errors
  | 0x05 -> Some `T2mi_errors
  | 0x09 -> Some `Part
  | 0x0B -> Some `Streams
  | 0xFD -> Some `End_of_errors
  | 0xFF -> Some `End_of_transmission
  | _ -> None

let split_message (has_crc : bool) (buf : Cstruct.t) (tag : rsp_tag) =
  let length = match tag with
    | `Devinfo -> sizeof_common_header + sizeof_board_info
    | `Mode -> sizeof_common_header + sizeof_board_mode
    | `Status -> sizeof_common_header + sizeof_status
    | `Ts_errors ->
      let body = Cstruct.shift buf sizeof_common_header in
      let length = 2 * get_ts_errors_length body in
      sizeof_common_header + 2 + length
    | `T2mi_errors ->
      let body = Cstruct.shift buf sizeof_common_header in
      let length = 2 * get_t2mi_errors_length body in
      sizeof_common_header + 2 + length
    | `Streams ->
      let body = Cstruct.shift buf sizeof_common_header in
      let length = 2 * get_streams_list_event_length body in
      sizeof_common_header + 2 + length
    | `Part ->
      let body = Cstruct.shift buf sizeof_common_header in
      let length = 2 * get_complex_rsp_header_length body in
      sizeof_common_header + 2 + length
    | `End_of_errors -> sizeof_common_header + sizeof_end_of_errors
    | `End_of_transmission -> sizeof_common_header in
  Cstruct.split buf (if has_crc then length + 2 else length)

let complex_tag_to_enum : complex_tag -> int = function
  | `Deverr -> 0x0110
  | `Reset -> 0x0111
  | `Section -> 0x0302
  | `T2mi_seq -> 0x0306
  | `Jitter -> 0x0307
  | `Structure -> 0x0309
  | `Bitrate -> 0x030A
  | `T2mi_info -> 0x030B

let complex_tag_of_enum : int -> complex_tag option = function
  | 0x0110 -> Some `Deverr
  | 0x0111 -> Some `Reset
  | 0x0302 -> Some `Section
  | 0x0306 -> Some `T2mi_seq
  | 0x0307 -> Some `Jitter
  | 0x0309 -> Some `Structure
  | 0x030A -> Some `Bitrate
  | 0x030B -> Some `T2mi_info
  | _ -> None

type 'a simple_msg =
  { tag : 'a
  ; data : Cstruct.t
  }

type complex_msg =
  { tag : complex_tag
  ; client_id : int
  ; request_id : int
  ; data : Cstruct.t
  }

let simple_msg_to_string f m =
  Format.asprintf "tag: %s, data: %a"
    (f m.tag)
    Cstruct.hexdump_pp
    m.data

let make_complex_msg ?(request_id = 0) ?(client_id = 0)
    ?(data = Cstruct.empty) tag =
  { tag
  ; client_id
  ; request_id
  ; data
  }

type 'a msg =
  [ `Complex of complex_msg
  | `Simple of 'a simple_msg
  ]

type rsp = rsp_tag msg

type t2mi_mode_raw =
  { enabled : bool
  ; pid : int
  ; t2mi_stream_id : int
  ; stream : Stream.Multi_TS_ID.t
  } [@@deriving eq]

type jitter_raw =
  { measures : Jitter.measures
  ; next_ptr : int32
  ; time : int
  ; timestamp : Time.t
  ; pid : int
  ; t_pcr : float
  }

type structure =
  { info : Ts_info.t
  ; services : Service.t list
  ; tables : SI_PSI_table.t list
  ; pids : Pid.t list
  ; time : Time.t
  } [@@deriving eq]

type _ t =
  | Get_devinfo : devinfo t
  | Get_deverr : int -> Board_error.t list t
  | Get_mode : (input * t2mi_mode_raw) t
  | Set_mode : input * t2mi_mode_raw -> unit t
  | Set_jitter_mode : jitter_mode option -> unit t
  | Reset : unit t
  | Set_src_id :
      { input : int
      ; t2mi : int } -> unit t
  | Get_t2mi_seq :
      { request_id : int
      ; stream : Stream.ID.t
      ; seconds : int
      } -> T2mi_sequence.t ts t
  | Get_section :
      { request_id : int
      ; stream_id : Stream.Multi_TS_ID.t
      ; table_id : int
      ; table_id_ext : int option
      ; id_ext_1 : int option (* ts_id for EIT, orig_nw_id for SDT *)
      ; id_ext_2 : int option (* orig_nw_id for EIT *)
      ; section : int option
      } -> SI_PSI_section.Dump.t ts t
  | Get_jitter :
      { request_id : int
      ; pointer : int32
      } -> jitter_raw t
  | Get_bitrate : int -> (Stream.Multi_TS_ID.t * Bitrate.t) list t
  | Get_structure :
      { request_id : int
      ; stream : [ `All | `Single of Stream.Multi_TS_ID.t ]
      } -> (Stream.Multi_TS_ID.t * structure) list t
  | Get_t2mi_info :
      { request_id : int
      ; t2mi_stream_id : int
      ; stream : Stream.Multi_TS_ID.t
      } -> (Stream.Multi_TS_ID.t * T2mi_info.t) t

(* FIXME *)
let timeout (type a) : a t -> float = function
  | Get_devinfo -> 3.
  | Reset -> 0.
  | Set_src_id _ -> 0.
  | Get_deverr _ -> 3.
  | Get_mode -> 3.
  | Get_t2mi_seq x -> 0.
  | Get_section _ -> 0.
  | Get_jitter _ -> 0.
  | Get_bitrate _ -> 0.
  | Get_structure _ -> 0.
  | Get_t2mi_info _ -> 0.
  | Set_mode _ -> 0.
  | Set_jitter_mode _ -> 0.

let value_to_string (type a) (t : a t) (v : a) : string option =
  match t with
  | Get_devinfo -> Some (devinfo_to_string v)
  | Reset -> None
  | Set_src_id _ -> None
  | Get_deverr _ -> None
  | Get_mode -> None
  | Get_t2mi_seq _ -> None
  | Get_section _ -> None
  | Get_jitter _ -> None
  | Get_bitrate _ -> None
  | Get_structure _ -> None
  | Get_t2mi_info _ -> None
  | Set_mode _ -> None
  | Set_jitter_mode _ -> None

let to_string (type a) : a t -> string = function
  | Get_devinfo -> "Get devinfo"
  | Reset -> "Reset"
  | Set_src_id _ -> "Set source ID"
  | Get_deverr _ -> "Get deverr"
  | Get_mode -> "Get mode"
  | Get_t2mi_seq _ -> "Get T2-MI sequence"
  | Get_section _ -> "Get section"
  | Get_jitter _ -> "Get jitter"
  | Get_bitrate _ -> "Get bitrate"
  | Get_structure _ -> "Get structure"
  | Get_t2mi_info _ -> "Get T2-MI info"
  | Set_mode _ -> "Set mode"
  | Set_jitter_mode _ -> "Set jitter mode"
