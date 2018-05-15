open Common.Dvb_t2_types
open Common.Topology

(* Board info *)

type devinfo =
  { typ : int
  ; ver : int
  } [@@deriving yojson]

(* Board mode *)

type mode =
  { input : input
  ; t2mi  : t2mi_mode option
  }
and t2mi_mode =
  { enabled        : bool
  ; pid            : int
  ; t2mi_stream_id : int
  ; stream         : Common.Stream.id
  }
and input = SPI | ASI [@@deriving yojson,eq]

type jitter_mode =
  { stream  : Common.Stream.id
  ; pid     : int
  } [@@deriving yojson]

(* Status *)

type user_status =
  { load            : float
  ; reset           : bool
  ; mode            : mode
  ; jitter_mode     : jitter_mode option
  ; ts_num          : int
  ; services_num    : int
  ; bitrate         : int
  ; packet_sz       : packet_sz
  ; has_stream      : bool
  }
and packet_sz = Ts188
              | Ts192
              | Ts204 [@@deriving yojson]

type status_versions =
  { streams_ver  : int
  ; ts_ver_com   : int
  ; ts_ver_lst   : int list
  ; t2mi_ver_lst : int list
  }

type status =
  { status    : user_status
  ; errors    : bool
  ; t2mi_sync : int list
  ; versions  : status_versions
  ; streams   : Common.Stream.id list
  }

(* MPEG-TS errors *)

type ts_error =
  { count     : int
  ; err_code  : int
  ; err_ext   : int
  ; multi_pid : bool
  ; pid       : int
  ; packet    : int32
  ; param_1   : int32
  ; param_2   : int32
  } [@@deriving yojson]

type ts_error_list = ts_error list [@@deriving yojson]

type ts_errors =
  { stream_id : Common.Stream.id
  ; timestamp : Common.Time.Seconds.t
  ; errors    : ts_error list
  } [@@deriving yojson]

(* T2-MI errors *)

(* type t2mi_parser_error = Format_p2_type *)
(*                        | Bad_fft_for_lite *)
(*                        | Bad_gi *)
(*                        | Bad_fft_gi_combination *)
(*                        | Bad_frames_number *)
(*                        | L1_conf_too_large *)
(*                        | L1_dyn_too_large *)
(*                        | L1_cur_too_small *)
(*                        | No_dyn_in_l1_cur *)
(*                        | Bad_l1_dyn_length *)
(*                        | L1_fut_too_small *)
(*                        | L1_fut_found_but_not_needed *)
(*                        | Bad_timestamp_length *)
(*                        | L1_cur_absent [@@deriving yojson] *)

(* type t2mi_stream_error = *)
(*   { err_code : int *)
(*   ; count    : int *)
(*   ; param    : int option *)
(*   } [@@deriving yojson] *)

(* type t2mi_err = Stream of t2mi_stream_error *)
(*               | Parser of t2mi_parser_error [@@deriving yojson] *)

(* type t2mi_error = *)
(*   { t2mi_stream_id : int *)
(*   ; error          : t2mi_err *)
(*   } [@@deriving yojson] *)

type ts_parser_error = Af_too_long_for_new_packet
                     | Af_too_long
                     | Pf_out_of_bounds
                     | Packet_intersection [@@deriving yojson]

type t2mi_error =
  { t2mi_stream_id : int
  ; err_code       : int
  ; count          : int option
  ; param          : int option
  } [@@deriving yojson]

type t2mi_errors =
  { stream_id        : Common.Stream.id
  ; t2mi_pid         : int
  ; sync             : int list
  ; ts_parser_errors : ts_parser_error list
  ; errors           : t2mi_error list
  } [@@deriving yojson]

(* Board errors *)

type board_errors =
  { count  : int32
  ; errors : board_error list
  }
and board_error = Unknown_request         of int32
                | Too_many_args           of int32
                | Msg_queue_overflow      of int32
                | Not_enough_memory       of int32
                | Total_packets_overflow  of int32
                | Tables_overflow         of int32
                | Sections_overflow       of int32
                | Table_list_overflow     of int32
                | Services_overflow       of int32
                | Es_overflow             of int32
                | Ecm_overflow            of int32
                | Emm_overflow            of int32
                | Section_array_not_found of int32
                | Dma_error               of int32
                | Pcr_freq_error          of int32
                | Packets_overflow        of int32
                | Streams_overflow        of int32 [@@deriving yojson]

(* T2-MI frames sequence *)

type t2mi_packet_common =
  { id          : int
  ; super_frame : int
  ; count       : int
  } [@@deriving yojson]

type t2mi_packet_common_with_frame =
  { common : t2mi_packet_common
  ; frame  : int
  } [@@deriving yojson]

type bb =
  { common : t2mi_packet_common
  ; frame  : int
  ; plp    : int
  } [@@deriving yojson]

type l1_current =
  { common        : t2mi_packet_common
  ; frame         : int
  ; dyn_cur_frame : int
  } [@@deriving yojson]

type l1_future =
  { common          : t2mi_packet_common
  ; frame           : int
  ; dyn_next_frame  : int
  ; dyn_next2_frame : int
  } [@@deriving yojson]

type t2mi_packet = BB                       of bb
                 | Aux_stream_iq_data       of t2mi_packet_common
                 | Arbitrary_cell_insertion of t2mi_packet_common_with_frame
                 | L1_current               of l1_current
                 | L1_future                of l1_future
                 | P2_bias_balancing_cells  of t2mi_packet_common_with_frame
                 | Timestamp                of t2mi_packet_common
                 | Individual_addressing    of t2mi_packet_common
                 | FEF_null                 of t2mi_packet_common
                 | FEF_iq                   of t2mi_packet_common
                 | FEF_composite            of t2mi_packet_common
                 | FEF_sub_part             of t2mi_packet_common
                 | Unknown                  of t2mi_packet_common [@@deriving yojson]

type t2mi_seq = t2mi_packet list [@@deriving yojson]

(* Jitter *)

type jitter =
  { pid         : int
  ; time        : int32
  ; next_ptr    : int32
  ; bitrate     : int32
  ; t_pcr       : int32
  ; packet_time : int32
  ; k_drift     : int32
  ; k_fo        : int32
  ; k_jitter    : int32
  ; values      : jitter_item list
  }
and jitter_item =
  { status   : int
  ; d_packet : int
  ; d_pcr    : int32
  ; drift    : int32
  ; fo       : int32
  ; jitter   : int
  }[@@deriving yojson]

(* TS struct *)

type pid =
  { pid       : int
  ; bitrate   : int option
  ; has_pts   : bool
  ; scrambled : bool
  ; present   : bool
  } [@@deriving yojson]

type es =
  { pid          : int
  ; bitrate      : int option
  ; has_pts      : bool
  ; es_type      : int
  ; es_stream_id : int
  } [@@deriving yojson]

type ecm =
  { pid       : int
  ; bitrate   : int option
  ; ca_sys_id : int
  } [@@deriving yojson]

type service =
  { id             : int
  ; bitrate        : int option
  ; name           : string
  ; provider_name  : string
  ; pmt_pid        : int
  ; pcr_pid        : int
  ; has_pmt        : bool
  ; has_sdt        : bool
  ; dscr           : bool
  ; list_dscr      : bool
  ; eit_schedule   : bool
  ; eit_pf         : bool
  ; free_ca_mode   : bool
  ; running_status : int
  ; service_type_1 : int
  ; service_type_2 : int
  ; es             : es list
  ; ecm            : ecm list
  } [@@deriving yojson]

type emm = ecm [@@deriving yojson]

type actual_other = Actual | Other [@@deriving yojson]

type eit_type = Present | Schedule [@@deriving yojson]

type table_section =
  { id       : int
  ; analyzed : bool
  ; length   : int
  } [@@deriving yojson]

type table_common =
  { version        : int
  ; bitrate        : int option
  ; id             : int
  ; pid            : int
  ; lsn            : int
  ; section_syntax : bool
  ; sections       : table_section list
  } [@@deriving yojson]

type pat =
  { common : table_common
  ; ts_id  : int
  } [@@deriving yojson]

type pmt =
  { common         : table_common
  ; program_number : int
  } [@@deriving yojson]

type nit =
  { common : table_common
  ; ts     : actual_other
  ; nw_id  : int
  } [@@deriving yojson]

type sdt =
  { common : table_common
  ; ts_id  : int
  ; ts     : actual_other
  } [@@deriving yojson]

type bat =
  { common     : table_common
  ; bouquet_id : int
  } [@@deriving yojson]

type eit_info =
  { ts_id         : int
  ; orig_nw_id    : int
  ; segment_lsn   : int
  ; last_table_id : int
  } [@@deriving yojson]

type eit =
  { common     : table_common
  ; service_id : int
  ; ts         : actual_other
  ; typ        : eit_type
  ; eit_info   : eit_info
  } [@@deriving yojson]

type table = PAT     of pat
           | CAT     of table_common
           | PMT     of pmt
           | TSDT    of table_common
           | NIT     of nit
           | SDT     of sdt
           | BAT     of bat
           | EIT     of eit
           | TDT     of table_common
           | RST     of table_common
           | ST      of table_common
           | TOT     of table_common
           | DIT     of table_common
           | SIT     of table_common
           | Unknown of table_common [@@deriving yojson]

type general_struct_block =
  { complete     : bool
  ; services_num : int
  ; nw_pid       : int
  ; ts_id        : int
  ; nw_id        : int
  ; orig_nw_id   : int
  ; nw_name      : string
  ; bouquet_name : string
  } [@@deriving yojson]

type ts_struct =
  { timestamp    : Common.Time.Seconds.t
  ; stream_id    : Common.Stream.id
  ; bitrate      : int option
  ; general      : general_struct_block
  ; pids         : pid list
  ; services     : service list
  ; emm          : emm list
  ; tables       : table list
  } [@@deriving yojson]

type ts_structs = ts_struct list [@@deriving yojson]

(* SI/PSI section *)

type section_request =
  { stream_id : Common.Stream.id
  ; table     : table
  ; section   : int
  }

type section_error = Zero_length
                   | Table_not_found
                   | Section_not_found
                   | Stream_not_found
                   | Unknown [@@deriving yojson]

type section =
  { stream_id : Common.Stream.id
  ; data      : string (* FIXME*)
  } [@@deriving yojson]

(* Bitrate *)

type pid_bitrate =
  { pid     : int
  ; bitrate : int
  }

type table_bitrate =
  { id             : int
  ; id_ext         : int
  ; fully_analyzed : bool
  ; section_syntax : bool
  ; eit_info       : (int * int) option
  ; bitrate        : int
  }

type bitrate =
  { stream_id  : Common.Stream.id
  ; ts_bitrate : int
  ; pids       : pid_bitrate list
  ; tables     : table_bitrate list
  ; timestamp  : Common.Time.Seconds.t
  }

type bitrates = bitrate list

(* T2-MI info *)

type t2mi_packet_type = BB
                      | Aux_stream_iq_data
                      | Arbitrary_cell_insertion
                      | L1_current
                      | L1_future
                      | P2_bias_balancing_cells
                      | Timestamp
                      | Individual_addressing
                      | FEF_null
                      | FEF_iq
                      | FEF_composite
                      | FEF_sub_part
                      | Unknown [@@deriving yojson]

type l1_preamble = T2 of t2_profile
                 | Non_t2
                 | Unknown of int [@@deriving yojson]

type l1_pre =
  { typ                : t2_streams_type
  ; preamble           : l1_preamble
  ; fft                : t2_fft option
  ; mixed_flag         : bool
  ; bwt_ext            : bool
  ; s1                 : int
  ; s2                 : int
  ; l1_repetition_flag : bool
  ; guard_interval     : t2_gi
  ; papr               : t2_papr
  ; l1_mod             : t2_l1_mod
  ; l1_cod             : t2_l1_cod
  ; l1_fec_type        : t2_l1_fec
  ; l1_post_size       : int
  ; l1_post_info_size  : int
  ; pilot_pattern      : t2_pp
  ; tx_id_availability : int
  ; cell_id            : int
  ; network_id         : int
  ; t2_system_id       : int
  ; num_t2_frames      : int
  ; num_data_symbols   : int
  ; regen_flag         : int
  ; l1_post_extension  : bool
  ; num_rf             : int
  ; current_rf_idx     : int
  ; t2_version         : t2_version
  ; l1_post_scrambled  : bool
  ; t2_base_lite       : bool
  ; reserved           : int
  } [@@deriving yojson]

type t2_l1_post_conf_rf =
  { rf_idx    : int
  ; frequency : int
  } [@@deriving yojson]

type t2_l1_post_conf_fef =
  { fef_type     : int
  ; fef_length   : int
  ; fef_interval : int
  } [@@deriving yojson]

type t2_l1_post_conf_plp =
  { plp_id              : int
  ; plp_type            : t2_plp_type
  ; plp_payload_type    : t2_plp_payload_type
  ; ff_flag             : bool
  ; first_rf_idx        : int
  ; first_frame_idx     : int
  ; plp_group_id        : int
  ; plp_cod             : t2_plp_cod
  ; plp_mod             : t2_plp_mod
  ; plp_rotation        : bool
  ; plp_fec_type        : t2_plp_fec
  ; plp_num_blocks_max  : int
  ; frame_interval      : int
  ; time_il_length      : int
  ; time_il_type        : bool
  ; in_band_a_flag      : bool
  ; in_band_b_flag      : bool
  ; reserved_1          : int
  ; plp_mode            : t2_plp_mode
  ; static_flag         : bool
  ; static_padding_flag : bool
  } [@@deriving yojson]

type t2_l1_post_conf_aux =
  { aux_stream_type  : t2_aux_stream_type
  ; aux_private_conf : int
  } [@@deriving yojson]

type l1_post_conf =
  { sub_slices_per_frame  : int
  ; aux_config_rfu        : int
  ; rf                    : t2_l1_post_conf_rf list
  ; fef                   : t2_l1_post_conf_fef option
  ; plp                   : t2_l1_post_conf_plp list
  ; fef_length_msb        : int
  ; reserved_2            : int
  ; aux                   : t2_l1_post_conf_aux list
  } [@@deriving yojson]

type t2mi_info =
  { packets        : t2mi_packet_type list
  ; t2mi_stream_id : int
  ; l1_pre         : l1_pre option
  ; l1_post_conf   : l1_post_conf option
  } [@@deriving yojson]

(* Streams list*)

type streams = Common.Stream.id list [@@deriving yojson]


type config = { mode        : mode
              ; jitter_mode : jitter_mode option
              } [@@deriving yojson]

let config_to_string c = Yojson.Safe.to_string @@ config_to_yojson c

let config_of_string s = config_of_yojson @@ Yojson.Safe.from_string s

let config_default =
  { mode        = { input = ASI; t2mi  = None }
  ; jitter_mode = None
  }

(* L1 deserialization *)

(* FFT *)

let t2_fft_of_int t2_profile = function
  | 0b000      -> FFT_2K
  | 0b001      -> FFT_8K
  | 0b010      -> FFT_4K
  | 0b011      -> (match t2_profile with
                   | Base _ -> FFT_1K
                   | Lite _ -> FFT_16K)
  | 0b100      -> FFT_16K
  | 0b101 as x -> (match t2_profile with
                   | Base _ -> FFT_32K
                   | Lite _ -> Unknown x)
  | 0b110      -> FFT_8K
  | 0b111 as x -> (match t2_profile with
                   | Base _ -> FFT_32K
                   | Lite _ -> Unknown x)
  | x          -> Unknown x

(* L1 preamble *)

let l1_preamble_of_int = function
  | 0b000 -> T2 (Base SISO) | 0b001 -> T2 (Base MISO) | 0b010 -> Non_t2
  | 0b011 -> T2 (Lite SISO) | 0b100 -> T2 (Lite MISO) | x -> Unknown x

let l1_preamble_to_int = function
  | T2 (Base SISO) -> 0b000 | T2 (Base MISO) -> 0b001 | Non_t2 -> 0b010
  | T2 (Lite SISO) -> 0b011 | T2 (Lite MISO) -> 0b100 | Unknown x -> x

(* Streams type *)

let t2_streams_type_of_int : int -> t2_streams_type = function
  | 0x00 -> TS | 0x01 -> GS | 0x02 -> Both | x -> Unknown x

let t2_streams_type_to_int : t2_streams_type -> int = function
  | TS -> 0x00 | GS -> 0x01 | Both -> 0x02 | Unknown x -> x

(* Guard interval *)

let t2_gi_of_int = function
  | 0b000 -> GI_1_32   | 0b001 -> GI_1_16   | 0b010 -> GI_1_8
  | 0b011 -> GI_1_4    | 0b100 -> GI_1_128  | 0b101 -> GI_19_128
  | 0b110 -> GI_19_256 | x     -> Unknown x

let t2_gi_to_int = function
  | GI_1_32   -> 0b000 | GI_1_16   -> 0b001 | GI_1_8    -> 0b010
  | GI_1_4    -> 0b011 | GI_1_128  -> 0b100 | GI_19_128 -> 0b101
  | GI_19_256 -> 0b110 | Unknown x -> x

(* PAPR *)

let t2_papr_legacy_of_int = function
  | 0b0000 -> Off | 0b0001 -> ACE | 0b0010 -> TR | 0b0011 -> ACE_TR | x -> Unknown x

let t2_papr_legacy_to_int = function
  | Off -> 0b0000 | ACE -> 0b0001 | TR -> 0b0010 | ACE_TR -> 0b0011 | Unknown x -> x

let t2_papr_modern_of_int = function
  | 0b0000 -> L1_ACE_TR_P2 | 0b0001 -> L1_ACE_ACE | 0b0010 -> L1_ACE_TR | 0b0011 -> L1_ACE_ACE_TR | x -> Unknown x

let t2_papr_modern_to_int = function
  | L1_ACE_TR_P2 -> 0b0000 | L1_ACE_ACE -> 0b0001 | L1_ACE_TR -> 0b0010 | L1_ACE_ACE_TR -> 0b0011 | Unknown x -> x

let t2_papr_of_int version x =
  match version with
  | V1_1_1 -> Legacy (t2_papr_legacy_of_int x)
  | _      -> Modern (t2_papr_modern_of_int x)

let t2_papr_to_int = function
  | Legacy x -> t2_papr_legacy_to_int x
  | Modern x -> t2_papr_modern_to_int x

(* L1 modulation *)

let t2_l1_mod_of_int = function
  | 0b0000 -> BPSK | 0b0001 -> QPSK | 0b0010 -> QAM16 | 0b0011 -> QAM64 | x -> Unknown x

let t2_l1_mod_to_int = function
  | BPSK -> 0b0000 | QPSK -> 0b0001 | QAM16 -> 0b0010 | QAM64 -> 0b0011 | Unknown x -> x

(* L1 code rate *)

let t2_l1_cod_of_int : int -> t2_l1_cod = function
  | 0b00 -> CR_1_2 | x -> Unknown x

let t2_l1_cod_to_int : t2_l1_cod -> int = function
  | CR_1_2 -> 0b00 | Unknown x -> x

(* L1 fec *)

let t2_l1_fec_of_int : int -> t2_l1_fec= function
  | 0b00 -> LDPC_16K | x -> Unknown x

let t2_l1_fec_to_int : t2_l1_fec -> int = function
  | LDPC_16K -> 0b00 | Unknown x -> x


(* Pilot pattern *)

let t2_pp_of_int = function
  | 0b0000 -> PP1 | 0b0001 -> PP2 | 0b0010 -> PP3 | 0b0011 -> PP4
  | 0b0100 -> PP5 | 0b0101 -> PP6 | 0b0110 -> PP7 | 0b0111 -> PP8
  | x -> Unknown x

let t2_pp_to_int = function
  | PP1 -> 0b0000 | PP2 -> 0b0001 | PP3 -> 0b0010 | PP4 -> 0b0011
  | PP5 -> 0b0100 | PP6 -> 0b0101 | PP7 -> 0b0110 | PP8 -> 0b0111
  | Unknown x -> x

(* T2 version *)

let t2_version_of_int = function
  | 0b0000 -> V1_1_1 | 0b0001 -> V1_2_1 | 0b0010 -> V1_3_1 | x -> Unknown x

let t2_version_to_int = function
  | V1_1_1 -> 0b0000 | V1_2_1 -> 0b0001 | V1_3_1 -> 0b0010 | Unknown x -> x


(* PLP type *)

let t2_plp_type_of_int = function
  | 0b000 -> Common | 0b001 -> Data_type_1 | 0b010 -> Data_type_2 | x -> Unknown x

let t2_plp_type_to_int = function
  | Common -> 0b000 | Data_type_1 -> 0b001 | Data_type_2 -> 0b010 | Unknown x -> x

(* PLP payload type *)

let t2_plp_payload_type_of_int = function
  | 0b00000 -> GFPS | 0b00001 -> GCS | 0b00010 -> GSE | 0b00011 -> TS | x -> Unknown x

let t2_plp_payload_type_to_int = function
  | GFPS -> 0b00000 | GCS -> 0b00001 | GSE -> 0b00010 | TS -> 0b00011 | Unknown x -> x

(* PLP code rate *)

let t2_plp_cod_of_int t2_profile = function
  | 0b000      -> CR_1_2
  | 0b001      -> CR_3_5
  | 0b010      -> CR_2_3
  | 0b011      -> CR_3_4
  | 0b100 as x -> (match t2_profile with
                   | Base _ -> CR_4_5
                   | Lite _ -> Unknown x)
  | 0b101 as x -> (match t2_profile with
                   | Base _ -> CR_5_6
                   | Lite _ -> Unknown x)
  | 0b110 as x -> (match t2_profile with
                   | Lite _ -> CR_1_3
                   | Base _ -> Unknown x)
  | 0b111 as x -> (match t2_profile with
                   | Lite _ -> CR_2_5
                   | Base _ -> Unknown x)
  | x          -> Unknown x

let t2_plp_cod_to_int = function
  | CR_1_2 -> 0b000 | CR_3_5 -> 0b001 | CR_2_3 -> 0b010
  | CR_3_4 -> 0b011 | CR_4_5 -> 0b100 | CR_5_6 -> 0b101
  | CR_1_3 -> 0b110 | CR_2_5 -> 0b111 | Unknown x -> x


(* PLP modulation *)

let t2_plp_mod_of_int = function
  | 0b000 -> QPSK | 0b001 -> QAM16 | 0b010 -> QAM64 | 0b011 -> QAM256 | x -> Unknown x

let t2_plp_mod_to_int = function
  | QPSK -> 0b000 | QAM16 -> 0b001 | QAM64 -> 0b010 | QAM256 -> 0b011 | Unknown x -> x

(* PLP fec type *)

let t2_plp_fec_of_int t2_profile = function
  | 0b00      -> LDPC_16K
  | 0b01 as x -> (match t2_profile with
                  | Base _ -> LDPC_64K
                  | Lite _ -> Unknown x)
  | x          -> Unknown x

let t2_plp_fec_to_int = function
  | LDPC_16K -> 0b00 | LDPC_64K -> 0b01 | Unknown x -> x


(* PLP mode *)

let t2_plp_mode_of_int = function
  | 0b00 -> Not_specified | 0b01 -> NM | 0b10 -> HEM | x -> Unknown x

let t2_plp_mode_to_int = function
  | Not_specified -> 0b00 | NM -> 0b01 | HEM -> 0b10 | Unknown x -> x

(* Aux stream type *)

let aux_stream_type_of_int = function
  | 0b0000 -> TX_SIG | x -> Unknown x

let aux_stream_type_to_int = function
  | TX_SIG -> 0xb0000 | Unknown x -> x


type devinfo_response    = devinfo option [@@deriving yojson]
type mode_request        = mode [@@deriving yojson]
type t2mi_mode_request   = t2mi_mode option [@@deriving yojson]
type jitter_mode_request = jitter_mode option [@@deriving yojson]
type t2mi_seq_response   = t2mi_packet list [@@deriving yojson]

let table_common_of_table = function
  | PAT x     -> x.common | CAT x     -> x        | PMT x     -> x.common
  | TSDT x    -> x        | NIT x     -> x.common | SDT x     -> x.common
  | BAT x     -> x.common | EIT x     -> x.common | TDT x     -> x
  | RST x     -> x        | ST  x     -> x        | TOT x     -> x
  | DIT x     -> x        | SIT x     -> x        | Unknown x -> x

let table_to_string = function
  | PAT _     -> "PAT"
  | CAT _     -> "CAT"
  | PMT _     -> "PMT"
  | TSDT _    -> "TSDT"
  | NIT x     -> (match x.ts with
                  | Actual -> "NIT actual"
                  | Other  -> "NIT other")
  | SDT x     -> (match x.ts with
                  | Actual -> "SDT actual"
                  | Other  -> "SDT other")
  | BAT _     -> "BAT"
  | EIT x     -> (match x.ts,x.typ with
                  | Actual,Present  -> "EIT actual present"
                  | Other, Present  -> "EIT other present"
                  | Actual,Schedule -> "EIT actual schedule"
                  | Other, Schedule -> "EIT other schedule")
  | TDT _     -> "TDT"
  | RST _     -> "RST"
  | ST  _     -> "ST"
  | TOT _     -> "TOT"
  | DIT _     -> "DIT"
  | SIT _     -> "SIT"
  | Unknown _ -> "Unknown"
