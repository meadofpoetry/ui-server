open Common.Dvb_t2_types
open Common.Hardware

(* Board info *)

type info =
  { typ : int
  ; ver : int
  } [@@deriving yojson]

(* Board mode *)

type input =
   | SPI
   | ASI [@@deriving yojson]

type t2mi_mode =
  { enabled   : bool
  ; pid       : int
  ; stream_id : Common.Stream.t
  } [@@deriving yojson]

type mode =
  { input : input
  ; t2mi  : t2mi_mode option
  } [@@deriving yojson]

type jitter_mode =
  { stream_id : Common.Stream.t
  ; pid       : int} [@@deriving yojson]

(* Status *)

type packet_sz = Ts188
               | Ts192
               | Ts204 [@@deriving yojson]

type user_status =
  { load            : float
  ; mode            : mode
  ; jitter_mode     : jitter_mode
  ; ts_num          : int
  ; services_num    : int
  ; bitrate         : int
  ; packet_sz       : packet_sz
  ; has_stream      : bool
  } [@@deriving yojson]

type status_versions =
  { streams_ver  : int
  ; ts_ver_com   : int
  ; ts_ver_lst   : int list
  ; t2mi_ver_lst : int list
  } [@@deriving yojson]

type status =
  { status    : user_status
  ; errors    : bool
  ; reset     : bool
  ; t2mi_sync : int list
  ; versions  : status_versions
  ; streams   : Common.Stream.t list
  } [@@deriving yojson]

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

type ts_errors =
  { stream_id : Common.Stream.t
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
  { stream_id        : Common.Stream.t
  ; t2mi_pid         : int
  ; sync             : int list
  ; ts_parser_errors : ts_parser_error list
  ; errors           : t2mi_error list
  } [@@deriving yojson]

(* Board errors *)

type board_error = Unknown_request         of int32
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

type board_errors =
  { count  : int32
  ; errors : board_error list
  } [@@deriving yojson]

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

(* Jitter *)

type jitter_item =
  { status   : int
  ; d_packet : int
  ; d_pcr    : int32
  ; drift    : int32
  ; fo       : int32
  ; jitter   : int
  } [@@deriving yojson]

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
  } [@@deriving yojson]

(* TS struct *)

type pid =
  { pid       : int
  ; has_pts   : bool
  ; scrambled : bool
  ; present   : bool
  } [@@deriving yojson]

type es =
  { pid          : int
  ; has_pts      : bool
  ; es_type      : int
  ; es_stream_id : int
  } [@@deriving yojson]

type ecm =
  { pid       : int
  ; ca_sys_id : int
  } [@@deriving yojson]

type service =
  { id            : int
  ; name          : string
  ; provider_name : string
  ; pmt_pid       : int
  ; pcr_pid       : int
  ; has_pmt       : bool
  ; has_sdt       : bool
  ; dscr          : bool
  ; list_dscr     : bool
  ; eit_schedule  : bool
  ; eit_pf        : bool
  ; free_ca_mode  : bool
  ; es            : es list
  ; ecm           : ecm list
  } [@@deriving yojson]

type emm = ecm [@@deriving yojson]

type table_section =
  { id       : int
  ; analyzed : bool
  ; length   : int
  } [@@deriving yojson]

type table_info_common =
  { version        : int
  ; id             : int
  ; pid            : int
  ; lsn            : int
  ; section_syntax : bool
  ; sections       : table_section list
  } [@@deriving yojson]

type pat =
  { common : table_info_common
  ; ts_id  : int
  } [@@deriving yojson]

type pmt =
  { common         : table_info_common
  ; program_number : int
  } [@@deriving yojson]

type nit =
  { common : table_info_common
  ; nw_id  : int
  } [@@deriving yojson]

type sdt = pat [@@deriving yojson]

type bat =
  { common     : table_info_common
  ; bouquet_id : int
  } [@@deriving yojson]

type eit_info =
  { ts_id         : int
  ; orig_nw_id    : int
  ; segment_lsn   : int
  ; last_table_id : int
  } [@@deriving yojson]

type eit =
  { common     : table_info_common
  ; service_id : int
  ; eit_info   : eit_info
  } [@@deriving yojson]

type table = PAT    of pat
           | CAT    of table_info_common
           | PMT    of pmt
           | TSDT   of table_info_common
           | NIT_a  of nit
           | NIT_o  of nit
           | SDT_a  of sdt
           | SDT_o  of sdt
           | BAT    of bat
           | EIT_ap of eit
           | EIT_op of eit
           | EIT_as of eit
           | EIT_os of eit
           | TDT    of table_info_common
           | RST    of table_info_common
           | ST     of table_info_common
           | TOT    of table_info_common
           | DIT    of table_info_common
           | SIT    of table_info_common
           | Unknown of table_info_common [@@deriving yojson]

type general_struct_block =
  { complete     : bool
  ; services_num : int
  ; nw_pid       : int
  ; ts_id        : int
  ; nw_id        : int
  ; orig_nw_id   : int
  ; nw_name      : string
  } [@@deriving yojson]

type ts_struct =
  { stream_id    : Common.Stream.t
  ; general      : general_struct_block
  ; pids         : pid list
  ; services     : service list
  ; emm          : emm list
  ; tables       : table list
  } [@@deriving yojson]

(* SI/PSI section *)

type section_request =
  { stream_id : Common.Stream.t
  ; table     : table
  ; section   : int
  }

type section_error = Zero_length
                   | Table_not_found
                   | Section_not_found
                   | Stream_not_found
                   | Unknown [@@deriving yojson]

type section =
  { stream_id : Common.Stream.t
  ; data      : string (* FIXME*)
  } [@@deriving yojson]

(* Bitrate *)

type pid_bitrate =
  { pid     : int
  ; bitrate : int
  } [@@deriving yojson]

type table_bitrate =
  { id             : int
  ; id_ext         : int
  ; fully_analyzed : bool
  ; section_syntax : bool
  ; eit_info       : (int * int) option
  ; bitrate        : int
  } [@@deriving yojson]

type bitrate =
  { stream_id  : Common.Stream.t
  ; ts_bitrate : int
  ; pids       : pid_bitrate list
  ; tables     : table_bitrate list
  } [@@deriving yojson]

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

type l1_pre =
  { typ                : t2_streams_type
  ; bwt_ext            : bool
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
  ; reserved_1          : bool
  ; plp_mode            : t2_plp_mode
  ; static_flag         : bool
  ; static_padding_flag : bool
  } [@@deriving yojson]

type t2_aux_stream_type = TX_SIG
                        | Unknown of int [@@deriving yojson]

type t2_l1_post_conf_aux =
  { aux_stream_type  : t2_aux_stream_type
  ; aux_private_conf : int
  } [@@deriving yojson]

type l1_post_conf =
  { sub_sclices_per_frame : int
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

type streams = Common.Stream.t list [@@deriving yojson]


type config = { mode        : mode
              ; jitter_mode : jitter_mode
              } [@@deriving yojson]

let config_to_string c = Yojson.Safe.to_string @@ config_to_yojson c

let config_of_string s = config_of_yojson @@ Yojson.Safe.from_string s

let config_default =
  { mode        = { input = ASI
                  ; t2mi  = Some { enabled   = true
                                 ; pid       = 4096
                                 ; stream_id = Single }
                  }
  ; jitter_mode = { stream_id = Single
                  ; pid       = 65535
                  }
  }
