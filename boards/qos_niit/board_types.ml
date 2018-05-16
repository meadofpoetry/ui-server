open Common.Dvb_t2_types
open Common.Topology

include Structure_types

(** Board info **)

type devinfo =
  { typ : int
  ; ver : int
  } [@@deriving yojson]

(** Modes **)

type input = SPI | ASI [@@deriving yojson,eq]

type t2mi_mode =
  { enabled        : bool
  ; pid            : int
  ; t2mi_stream_id : int
  ; stream         : Common.Stream.id (* NOTE maybe t? *)
  } [@@deriving yojson]

type jitter_mode =
  { stream  : Common.Stream.id (* NOTE maybe t? *)
  ; pid     : int
  } [@@deriving yojson]

(** Status **)

type status =
  { load            : float
  ; reset           : bool
  ; input           : input
  ; t2mi_mode       : t2mi_mode option
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

(** MPEG-TS errors **)

type crc_err_param =
  { computed : int32
  ; actual   : int32
  }

type table_info_param =
  { section      : int
  ; table_id     : int
  ; table_id_ext : int
  }

type period_err_param = Ptime.t

type pat_err = Scrambled
             | Table_id       of int
             | Long_interval  of period_err_param * table_info_param
             | CRC            of crc_err_param

type cc_err = Repetition
            | Loss
            | Order
            | Payload
            | Not_identical

type pmt_err = pat_err

type pcr_err = Long_interval of period_err_param
             | Discontinuity of period_err_param
             | Loss          of int

type cat_err = Scrambled
             | Table_id  of int
             | CRC       of crc_err_param
             | Missing

type nit_err = Table_id      of int
             | Long_interval of period_err_param * table_info_param
             | Scrambled

type si_rep_err = Short_interval of period_err_param * table_info_param
                | Long_interval  of period_err_param * table_info_param

type sdt_err = nit_err

type eit_err = Table_id      of int
             | Long_interval of period_err_param * table_info_param

type rst_err = Table_id  of int
             | Scrambled

type tdt_err = nit_err

type ts_err = TS_sync_loss
            | Sync_byte_error
            | PAT_error           of pat_err
            | CC_error            of cc_err
            | PMT_error           of pmt_err
            | PID_error
            | Transport_error
            | CRC_error           of crc_err_param
            | PCR_error           of pcr_err
            | PCR_accuracy_error  of period_err_param
            | PTS_error           of period_err_param
            | CAT_error           of cat_err
            | NIT_error           of nit_err
            | SI_repetition_error of si_rep_err
            | Unreferenced_pid
            | SDT_error           of sdt_err
            | EIT_error           of eit_err
            | RST_error           of rst_err
            | TDT_error           of tdt_err

let ts_error_name = function
  | TS_sync_loss          -> "TS sync loss"
  | Sync_byte_error       -> "Sync byte error"
  | PAT_error _           -> "PAT error"
  | CC_error _            -> "Continuity count error"
  | PMT_error _           -> "PMT error"
  | PID_error             -> "PID error"
  | Transport_error       -> "Transport error"
  | CRC_error _           -> "CRC error"
  | PCR_error _           -> "PCR error"
  | PCR_accuracy_error _  -> "PCR accuracy error"
  | PTS_error _           -> "PTS error"
  | CAT_error _           -> "CAT error"
  | NIT_error _           -> "NIT error"
  | SI_repetition_error _ -> "SI repetition error"
  | Unreferenced_pid      -> "Unreferenced pid"
  | SDT_error _           -> "SDT error"
  | EIT_error _           -> "EIT error"
  | RST_error _           -> "RST error"
  | TDT_error _           -> "TDT error"

let ts_error_priority = function
  | (TS_sync_loss | Sync_byte_error | PAT_error _
     | CC_error _ | PMT_error _     | PID_error)         -> `P1
  | (Transport_error        | CRC_error _ | PCR_error _
     | PCR_accuracy_error _ | PTS_error _ | CAT_error _) -> `P2
  | (NIT_error _   | SI_repetition_error _ | Unreferenced_pid
     | SDT_error _ | EIT_error _           | RST_error _
     | TDT_error _)                                      -> `P3

type ts_error =
  { count     : int
  ; err_code  : int
  ; err_ext   : int
  ; multi_pid : bool
  ; pid       : int
  ; packet    : int32
  ; param_1   : int32
  ; param_2   : int32
  } [@@Deriving yojson]

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

(** Board errors **)

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

(** T2-MI frames sequence **)

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

(** Jitter **)

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

(** SI/PSI section **)

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

(** T2-MI info **)

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

type l1_preamble = T2      of t2_profile
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

module Errors_api = struct

  open Common.Time

  type percentage =
    { errors  : float
    ; no_sync : float
    ; no_data : float
    } [@@deriving yojson]

  type priority = [ `Ts   of [`P1 | `P2 | `P3 ]
                  | `T2mi of [`Container | `T2MI]
                  ] [@@deriving yojson]

  type error_id =
    { err_code : int
    ; err_ext  : int
    } [@@deriving yojson]

  type ts_errors_response_archive =
    { errors  : ts_errors list
    ; no_data : Interval.Seconds.t list
    ; no_sync : Interval.Seconds.t list
    } [@@deriving yojson]

end

type config =
  { input       : input
  ; t2mi_mode   : t2mi_mode option
  ; jitter_mode : jitter_mode option
  } [@@deriving yojson]

let config_to_string c = Yojson.Safe.to_string @@ config_to_yojson c
let config_of_string s = config_of_yojson @@ Yojson.Safe.from_string s

let config_default =
  { input       = ASI
  ; t2mi_mode   = None
  ; jitter_mode = None
  }

type devinfo_response    = devinfo option     [@@deriving yojson]
type t2mi_mode_request   = t2mi_mode option   [@@deriving yojson]
type jitter_mode_request = jitter_mode option [@@deriving yojson]
