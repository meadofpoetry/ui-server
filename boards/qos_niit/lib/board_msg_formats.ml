[@@@ocaml.warning "-32"]

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
 type complex_rsp_header =
   { length     : uint16_t
   ; client_id  : uint16_t
   ; code_ext   : uint16_t
   ; request_id : uint16_t
   ; param      : uint16_t
   } [@@little_endian]]

[%%cstruct
 type complex_rsp_header_ext =
   { length     : uint16_t
   ; client_id  : uint16_t
   ; code_ext   : uint16_t
   ; request_id : uint16_t
   ; param      : uint32_t
   } [@@little_endian]]

[%%cstruct
 type req_set_init =
   { input_src_id : uint8_t
   ; t2mi_src_id  : uint8_t
   ; rfu          : uint8_t [@len 254]
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
 type req_get_section =
   { stream_id    : uint32_t
   ; table_id     : uint8_t
   ; section      : uint8_t
   ; table_id_ext : uint16_t
   ; adv_info_1   : uint16_t
   ; adv_info_2   : uint16_t
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
   { rfu       : uint8_t
   ; stream_id : uint8_t
   } [@@little_endian]]

(* Status *)

[%%cstruct
 type status =
   { rfu                 : uint32_t
   ; ts_num              : uint8_t
   ; streams_ver         : uint8_t
   ; load                : uint8_t
   ; ts_ver_com          : uint8_t
   ; bitrate             : uint32_t
   ; mode                : uint8_t
   ; rfu_1               : uint8_t
   ; t2mi_pid            : uint16_t
   ; t2mi_stream_id      : uint32_t
   ; rfu_2               : uint16_t
   ; flags               : uint8_t
   ; services_num        : uint8_t
   ; t2mi_sync           : uint8_t
   ; rfu_3               : uint8_t [@len 7]
   ; flags_2             : uint16_t
   ; version             : uint16_t
   ; t2mi_ver_lst        : uint32_t
   ; rfu_5               : uint32_t [@len 7]
   ; ts_ver_lst          : uint8_t [@len 50]
   ; rfu_6               : uint8_t [@len 50]
   ; jitter_stream_id    : uint32_t
   ; jitter_pid          : uint16_t
   ; rfu_7               : uint8_t [@len 6]
   ; ts_absent_lst       : uint16_t [@len 8]
   ; ts_not_verified_lst : uint16_t [@len 8]
   ; rfu_8               : uint16_t [@len 146]
   } [@@little_endian]]

(* TS errors *)

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

(* T2-MI errors *)

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

(* Board errors *)

[%%cstruct
 type board_errors =
   { count  : uint32_t
   ; errors : uint32_t [@len 17]
   } [@@little_endian]]

(* Get section *)

[%%cstruct
 type section =
   { length    : uint16_t
   ; result    : uint16_t
   } [@@little_endian]]

(* T2-MI frame sequence*)

[%%cstruct
 type t2mi_frame_seq_item =
   { typ           : uint8_t
   ; sframe_stream : uint8_t
   ; frame         : uint8_t
   ; plp           : uint8_t
   ; dyn1_frame    : uint8_t
   ; dyn2_frame    : uint8_t
   ; count         : uint32_t
   ; time          : uint32_t
   } [@@little_endian]]

(* Jitter*)

[%%cstruct
 type jitter_item =
   { status : uint16_t
   ; d_packet : uint16_t
   ; d_pcr    : uint32_t
   ; drift    : uint32_t
   ; fo       : uint32_t
   ; jitter   : uint16_t
   ; rfu      : uint32_t [@len 10]
   } [@@little_endian]]

[%%cstruct
 type jitter =
   { count       : uint16_t
   ; pid         : uint16_t
   ; time        : uint32_t
   ; req_ptr     : uint32_t
   ; req_next    : uint32_t
   ; bitrate     : uint32_t
   ; t_pcr       : uint32_t
   ; packet_time : uint32_t
   ; rfu         : uint32_t
   ; k_drift     : uint32_t
   ; k_fo        : uint32_t
   ; k_jitter    : uint32_t
   ; rfu_1       : uint64_t
   } [@@little_endian]]

[%%cstruct
 type req_get_jitter =
   { ptr : uint32_t
   } [@@little_endian]]

(* Ts structures *)

[%%cstruct
 type struct_block_header =
   { code   : uint16_t
   ; length : uint16_t
   } [@@little_endian]]

[%%cstruct
 type general_struct_block =
   { services_num : uint8_t
   ; version      : uint8_t
   ; bitrate      : uint32_t
   ; packet_count : uint32_t
   ; network_pid  : uint16_t
   ; ts_id        : uint16_t
   ; nw_id        : uint16_t
   ; orig_nw_id   : uint16_t
   ; bouquet_id   : uint16_t
   ; nw_info      : uint8_t [@len 12]
   ; string_len   : uint16_t
   } [@@little_endian]]

[%%cstruct
 type services_struct_block =
   { id                : uint16_t
   ; pmt_pid           : uint16_t
   ; pcr_pid           : uint16_t
   ; flags             : uint16_t
   ; service_type      : uint8_t
   ; service_type_list : uint8_t
   ; bitrate           : uint32_t
   } [@@little_endian]]

[%%cstruct
 type es_struct_block =
   { pid          : uint16_t
   ; es_type      : uint8_t
   ; es_stream_id : uint8_t
   } [@@little_endian]]

[%%cstruct
 type ecm_struct_block =
   { pid          : uint16_t
   ; ca_system_id : uint8_t
   ; rfu          : uint8_t
   } [@@little_endian]]

[%%cstruct
 type table_struct_block =
   { version    : uint8_t
   ; id         : uint8_t
   ; id_ext     : uint16_t
   ; lsn        : uint8_t
   ; rfu        : uint8_t
   ; adv_info_1 : uint16_t
   ; adv_info_2 : uint16_t
   ; adv_info_3 : uint8_t
   ; adv_info_4 : uint8_t
   ; pid        : uint16_t
   } [@@little_endian]]

[%%cstruct
 type ts_struct =
   { length    : uint32_t
   ; stream_id : uint32_t
   } [@@little_endian]]

[%%cstruct
 type ts_structs =
   { count   : uint8_t
   ; version : uint8_t
   ; rfu     : uint8_t [@len 6]
   } [@@little_endian]]

(* Bitrates *)

[%%cstruct
 type pid_bitrate =
   { pid     : uint16_t
   ; packets : uint32_t
   } [@@little_endian]]

[%%cstruct
 type table_bitrate =
   { table_id     : uint8_t
   ; flags        : uint8_t
   ; table_id_ext : uint16_t
   ; adv_info_1   : uint16_t
   ; adv_info_2   : uint16_t
   ; packets      : uint32_t
   } [@@little_endian]]

[%%cstruct
 type stream_bitrate =
   { length        : uint32_t
   ; stream_id     : uint32_t
   ; ts_bitrate    : uint32_t
   ; total_packets : uint32_t
   ; total_pids    : uint16_t
   ; total_tables  : uint16_t
   } [@@little_endian]]

[%%cstruct
 type bitrates =
   { count   : uint8_t
   ; version : uint8_t
   ; rfu     : uint8_t [@len 6]
   } [@@little_endian]]

(* T2-MI info *)

[%%cstruct
 type t2mi_info =
   { version   : uint16_t
   ; rfu       : uint8_t [@len 3]
   ; stream_id : uint8_t
   ; packets   : uint8_t [@len 32]
   ; length    : uint16_t
   } [@@little_endian]]

[%%cstruct
 type t2mi_info_ext =
   { t2mi_pid  : uint16_t
   ; conf_len  : uint16_t
   ; l1_pre    : uint8_t [@len 21]
   } [@@little_endian]]

(* Streams list *)

[%%cstruct
 type streams_list =
   { count   : uint8_t
   ; version : uint8_t
   ; rfu     : uint16_t
   } [@@little_endian]]

(* Streams list event *)

[%%cstruct
 type streams_list_event =
   { length  : uint16_t
   ; count   : uint8_t
   ; version : uint8_t
   ; rfu     : uint16_t
   } [@@little_endian]]

(* Set jitter mode *)

[%%cstruct
 type req_set_jitter_mode =
   { pid       : uint16_t
   ; stream_id : uint32_t
   } [@@little_endian]]

[@@@ocaml.warning "+32"]
