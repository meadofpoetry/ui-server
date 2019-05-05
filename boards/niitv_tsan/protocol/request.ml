open Application_types
open Board_niitv_tsan_types
open Message

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

let rsp_tag_to_enum : rsp_tag -> int = function
  | `Devinfo -> 0x01
  | `Mode -> 0x02
  | `Status -> 0x03
  | `Ts_errors -> 0x04
  | `T2mi_errors -> 0x05
  | `Streams -> 0x0B
  | `End_of_errors -> 0xFD
  | `End_of_transmission -> 0xFF

let complex_tag_to_enum : complex_tag -> int = function
  | `Deverr -> 0x0110
  | `Reset -> 0x0111
  | `Section -> 0x0302
  | `T2mi_seq -> 0x0306
  | `Jitter -> 0x0307
  | `Structure -> 0x0309
  | `Bitrate -> 0x030A
  | `T2mi_info -> 0x030B

type 'a msg =
  { tag : 'a
  ; data : Cstruct.t
  }

type complex_msg =
  { tag : complex_tag
  ; client_id : int
  ; request_id : int
  ; data : Cstruct.t
  }

let make_complex_msg ?(request_id = 0) ?(client_id = 0)
    ?(data = Cstruct.empty) tag =
  { tag
  ; client_id
  ; request_id
  ; data
  }

type req_msg =
  [ `Complex of complex_msg
  | `Simple of req_tag msg
  ]

type error =
  | Timeout
  | Queue_overflow
  | Not_responding
  | Invalid_length
  | Invalid_payload

let error_to_string = function
  | Timeout -> "timeout"
  | Queue_overflow -> "queue overflow"
  | Not_responding -> "not responding"
  | Invalid_length -> "invalid length"
  | Invalid_payload -> "invalid payload"

type t2mi_mode_raw =
  { enabled : bool
  ; pid : int
  ; t2mi_stream_id : int
  ; stream : Stream.Multi_TS_ID.t
  } [@@deriving show, eq]

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
  } [@@deriving yojson, eq]

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
      } -> (SI_PSI_section.Dump.t ts, SI_PSI_section.Dump.error) result t
  | Get_jitter :
      { request_id : int
      ; pointer : int32
      } -> jitter_raw t
  | Get_bitrate : int -> (Stream.Multi_TS_ID.t * Bitrate.t) t
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
