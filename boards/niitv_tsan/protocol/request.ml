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
  | `Get_devinfo
  | `Get_mode
  ] [@@deriving eq]

type event_tag =
  [ `Status
  | `Streams
  | `Ts_errors
  | `T2mi_errors
  | `End_of_errors
  | `End_of_transmission
  ]

type response_tag =
  [ `Devinfo
  | `Mode
  | `Part
  ]

type simple_tag =
  [ event_tag
  | response_tag
  ]

type complex_tag =
  [ `Reset
  | `Jitter_mode
  | `Deverr
  | `Section
  | `T2mi_seq
  | `Jitter
  | `Bitrate
  | `Structure
  | `T2mi_info
  ] [@@deriving eq]

let req_tag_to_enum : req_tag -> int = function
  | `Get_devinfo -> 0x0080
  | `Get_mode -> 0x0081
  | `Set_mode -> 0x0082
  | `Set_source_id -> 0x0089

let event_tag_to_string : event_tag -> string = function
  | `Status -> "status"
  | `Streams -> "streams"
  | `Ts_errors -> "TS errors"
  | `T2mi_errors -> "T2-MI errors"
  | `End_of_errors -> "EOE"
  | `End_of_transmission -> "EOT"

let event_tag_to_enum : event_tag -> int = function
  | `Status -> 0x03
  | `Ts_errors -> 0x04
  | `T2mi_errors -> 0x05
  | `Streams -> 0x0B
  | `End_of_errors -> 0xFD
  | `End_of_transmission -> 0xFF

let event_tag_of_enum : int -> event_tag option = function
  | 0x03 -> Some `Status
  | 0x04 -> Some `Ts_errors
  | 0x05 -> Some `T2mi_errors
  | 0x0B -> Some `Streams
  | 0xFD -> Some `End_of_errors
  | 0xFF -> Some `End_of_transmission
  | _ -> None

let response_tag_to_string : response_tag -> string = function
  | `Devinfo -> "devinfo"
  | `Mode -> "mode"
  | `Part -> "part"

let response_tag_to_enum : response_tag -> int = function
  | `Devinfo -> 0x01
  | `Mode -> 0x02
  | `Part -> 0x09

let response_tag_of_enum : int -> response_tag option = function
  | 0x01 -> Some `Devinfo
  | 0x02 -> Some `Mode
  | 0x09 -> Some `Part
  | _ -> None

let simple_tag_to_string : simple_tag -> string = function
  | #event_tag as e -> event_tag_to_string e
  | #response_tag as r -> response_tag_to_string r

let simple_tag_to_enum : simple_tag -> int = function
  | #event_tag as e -> event_tag_to_enum e
  | #response_tag as r -> response_tag_to_enum r

let simple_tag_of_enum : int -> simple_tag option = fun i ->
  match event_tag_of_enum i with
  | None -> (response_tag_of_enum i :> simple_tag option)
  | Some tag -> Some (tag :> simple_tag)

let split_message (has_crc : bool) (buf : Cstruct.t) (tag : simple_tag) =
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
  | `Jitter_mode -> 0x0112
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

let complex_tag_to_string : complex_tag -> string = function
  | `Reset -> "Reset"
  | `Jitter_mode -> "Set jitter mode"
  | `Deverr -> "Get deverr"
  | `Section -> "Get section"
  | `T2mi_seq -> "Get T2-MI sequence"
  | `Jitter -> "Get jitter"
  | `Bitrate -> "Get bitrate"
  | `Structure -> "Get structure"
  | `T2mi_info -> "Get T2-MI info"

type 'a simple_msg =
  { tag : 'a
  ; body : Cstruct.t
  }

type complex_msg =
  { tag : complex_tag
  ; client_id : Uint16.t
  ; request_id : Uint16.t
  ; body : Cstruct.t
  }

let simple_msg_to_string f m =
  Format.asprintf "tag: %s, data: %a"
    (f m.tag)
    Cstruct.hexdump_pp
    m.body

let make_complex_msg
    ?(request_id = Uint16.zero)
    ?(client_id = Uint16.zero)
    ?(body = Cstruct.empty)
    tag =
  { tag
  ; client_id
  ; request_id
  ; body
  }

type 'a msg =
  [ `Complex of complex_msg
  | `Simple of 'a simple_msg
  ]

type rsp = simple_tag msg

type evt = event_tag simple_msg

type _ t =
  | Get_devinfo : devinfo t
  | Get_deverr :
      { request_id : Uint16.t
      ; timeout : float option
      } -> Deverr.t list t
  | Get_mode : (input * t2mi_mode) t
  | Set_mode :
      { input : input
      ; t2mi_mode : t2mi_mode
      } -> unit t
  | Set_jitter_mode : jitter_mode -> unit t
  | Reset : unit t
  | Set_src_id :
      { input_source : int
      ; t2mi_source : int } -> unit t
  | Get_t2mi_seq :
      { request_id : Uint16.t
      ; duration : int (* seconds (120 seconds max) *)
      } -> T2mi_sequence.t ts t
  | Get_section :
      { request_id : Uint16.t
      ; stream_id : Stream.Multi_TS_ID.t
      ; table_id : int
      ; table_id_ext : int option
      ; id_ext_1 : int option (* ts_id for EIT, orig_nw_id for SDT *)
      ; id_ext_2 : int option (* orig_nw_id for EIT *)
      ; section : int option
      } -> SI_PSI_section.Dump.t ts t
  | Get_bitrate :
      { request_id : Uint16.t
      } -> (Stream.Multi_TS_ID.t * Bitrate.t) list t
  | Get_structure :
      { request_id : Uint16.t
      ; stream : [ `All | `Single of Stream.Multi_TS_ID.t ]
      } -> (Stream.Multi_TS_ID.t * Structure.t) list t
  | Get_t2mi_info :
      { request_id : Uint16.t
      ; t2mi_stream_id : int
      } -> (int * T2mi_info.t) t

let to_enum (type a) : a t -> int = function
  | Get_devinfo -> 0
  | Get_deverr _ -> 1
  | Get_mode -> 2
  | Set_mode _ -> 3
  | Set_jitter_mode _ -> 4
  | Reset -> 5
  | Set_src_id _ -> 6
  | Get_t2mi_seq _ -> 7
  | Get_section _ -> 8
  | Get_bitrate _ -> 9
  | Get_structure _ -> 10
  | Get_t2mi_info _ -> 11

let timeout (type a) : a t -> float = function
  (* Responseless requests. If timeout if non-zero, it defines the time
     we wait for the status with the expected data. *)
  | Reset -> 5.
  | Set_mode _ -> 5.
  | Set_src_id _ -> 0.
  | Set_jitter_mode _ -> 5.
  (* Requests with responses *)
  | Get_devinfo -> 5.
  | Get_deverr { timeout = Some x; _ } -> x
  | Get_deverr { timeout = None; _ } -> 5.
  | Get_mode -> 5.
  | Get_t2mi_seq { duration; _ } -> 10. +. float_of_int duration
  | Get_section _ -> 125.
  | Get_bitrate _ -> 5.
  | Get_structure _ -> 5.
  | Get_t2mi_info _ -> 5.

let value_to_string (type a) (t : a t) (v : a) : string option =
  match t with
  | Get_devinfo -> Some (show_devinfo v)
  | Reset -> None
  | Set_src_id _ -> None
  | Get_deverr _ -> None
  | Get_mode -> None
  | Get_t2mi_seq _ -> None
  | Get_section _ -> None
  | Get_bitrate _ -> None
  | Get_structure _ -> None
  | Get_t2mi_info _ -> None
  | Set_mode _ -> None
  | Set_jitter_mode _ -> None

let pp_int_option out = function
  | None -> Format.pp_print_string out "None"
  | Some x -> Format.pp_print_int out x

let pp_float_option out = function
  | None -> Format.pp_print_string out "None"
  | Some x -> Format.pp_print_float out x

let to_string (type a) : a t -> string = function
  | Reset -> "Reset"
  | Get_mode -> "Get mode"
  | Get_devinfo -> "Get devinfo"
  | Set_src_id { input_source; t2mi_source } ->
    Printf.sprintf "Set source ID (input=%d, t2mi=%d)"
      input_source t2mi_source
  | Get_deverr { request_id; timeout } ->
    Format.asprintf "Get deverr (rid=%a, timeout=%a)"
      Uint16.pp request_id pp_float_option timeout
  | Get_t2mi_seq { request_id; duration } ->
    Format.asprintf "Get T2-MI sequence (rid=%a, duration=%d)"
      Uint16.pp request_id duration
  | Get_section { request_id; stream_id; table_id
                ; table_id_ext; id_ext_1; id_ext_2; section } ->
    Format.asprintf "Get section (rid=%a, stream=%a, table=%d, section=%a, \
                     table_ext=%a, id_ext_1=%a, id_ext_2=%a)"
      Uint16.pp request_id
      Stream.Multi_TS_ID.pp stream_id table_id
      pp_int_option section
      pp_int_option table_id_ext
      pp_int_option id_ext_1
      pp_int_option id_ext_2
  | Get_bitrate { request_id } ->
    Format.asprintf "Get bitrate (rid=%a)" Uint16.pp request_id
  | Get_structure { request_id; stream = `All } ->
    Format.asprintf "Get structure (rid=%a, all streams)"
      Uint16.pp request_id
  | Get_structure { request_id; stream = `Single s } ->
    Format.asprintf "Get structure (rid=%a, stream=%a)"
      Uint16.pp request_id Stream.Multi_TS_ID.pp s
  | Get_t2mi_info { request_id; t2mi_stream_id } ->
    Format.asprintf "Get T2-MI info (rid=%a, T2-MI stream ID=%d)"
      Uint16.pp request_id t2mi_stream_id
  | Set_mode { input; t2mi_mode = { enabled; pid; t2mi_stream_id; stream }} ->
    Format.asprintf "Set mode (input: %a, \
                     t2mi: enabled=%B, PID=%d, T2-MI stream ID=%d, stream=%a)"
      pp_input input
      enabled pid t2mi_stream_id
      pp_stream stream
  | Set_jitter_mode { stream; pid; _ } ->
    Format.asprintf "Set jitter mode (stream=%a, PID=%d)"
      Stream.Multi_TS_ID.pp stream pid
