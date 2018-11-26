open Common

(** Misc *)

type 'a timestamped =
  { timestamp : Time.t
  ; data : 'a
  } [@@deriving yojson, eq, show]

let make_timestamped timestamp data =
  { timestamp; data }

type 'a timespan =
  { from : Time.t
  ; till : Time.t
  ; data : 'a
  } [@@deriving yojson, eq]

let make_timespan ~from ~till data =
  { from; till; data }

(** Board info *)

type devinfo =
  { typ : int
  ; ver : int
  } [@@deriving yojson, eq]

(** Modes *)

type input =
  | SPI
  | ASI [@@deriving yojson, show, eq]

let input_to_string = function
  | SPI -> "SPI" | ASI -> "ASI"

let input_to_int = function
  | SPI -> 0
  | ASI -> 1
let input_of_int = function
  | 0 -> Some SPI
  | 1 -> Some ASI
  | _ -> None

type t2mi_mode =
  { enabled : bool
  ; pid : int
  ; t2mi_stream_id : int
  ; stream : Stream.t
  } [@@deriving yojson, eq, show]

type jitter_mode =
  { stream : Stream.Multi_TS_ID.t
  ; pid : int
  } [@@deriving yojson, eq, show]

(** Config *)

type config =
  { input : input
  ; t2mi_mode : t2mi_mode option
  ; jitter_mode : jitter_mode option
  } [@@deriving yojson,eq]

type packet_sz =
  | Ts188
  | Ts192
  | Ts204 [@@deriving show,eq]
let packet_sz_to_string : packet_sz -> string = function
  | Ts188 -> "Ts188"
  | Ts192 -> "Ts192"
  | Ts204 -> "Ts204"
let packet_sz_of_string_option : string -> packet_sz option = function
  | "Ts188" -> Some Ts188
  | "Ts192" -> Some Ts192
  | "Ts204" -> Some Ts204
  | _ -> None
let packet_sz_to_yojson x : Yojson.Safe.json =
  `String (packet_sz_to_string x)
let packet_sz_of_yojson = function
  | `String s ->
     begin match packet_sz_of_string_option s with
     | Some x -> Ok x
     | None -> Error (Printf.sprintf "packet_sz_of_yojson: bad string (%s)" s)
     end
  | x -> Error (Printf.sprintf "packet_sz_of_yojson: not string value (%s)"
                @@ Yojson.Safe.to_string x)

type status =
  { timestamp : Time.t
  ; load : float
  ; reset : bool
  ; ts_num : int
  ; services_num : int
  ; bitrate : int
  ; packet_sz : packet_sz
  ; has_sync : bool
  ; has_stream : bool
  } [@@deriving yojson, show, eq]

type reset_ts =
  { timestamp : Time.t
  }

(** Board errors **)

type board_error =
  { timestamp : Time.t
  ; err_code : int
  ; count : int
  } [@@deriving yojson, show]

(** Device state *)
type state =
  { state : Common.Topology.state
  ; from : Time.t
  ; till : Time.t
  } [@@deriving yojson, show]

type state_compressed =
  { no_response : float
  ; init : float
  ; fine : float
  } [@@deriving yojson, show]

module Jitter = struct

  type session =
    { timestamp : Time.t
    ; t_pcr : float
    ; mode : jitter_mode
    } [@@deriving yojson,eq]

  type measure =
    { discont_err : bool
    ; discont_ok : bool
    ; t_pcr : float
    (* for charts *)
    ; accuracy : float
    ; jitter : int
    ; drift : float
    ; fo : float
    ; period : float
    } [@@deriving yojson]

  type measures = measure list [@@deriving yojson]

  type archive_item =
    { session : session
    ; measures : measure list
    } [@@deriving yojson]

  type archive = archive_item list [@@deriving yojson]

end

module Bitrate = struct

  type table =
    { table_id : int
    ; table_id_ext : int
    ; id_ext_1 : int
    ; id_ext_2 : int
    ; fully_analyzed : bool
    ; section_syntax : bool
    ; bitrate : int
    } [@@deriving yojson, eq]

  type t =
    { total : int
    ; tables : table list
    ; pids : (int * int) list
    } [@@deriving yojson, eq]


end

module Ts_info = struct

  type t =
    { complete : bool
    ; services_num : int
    ; nw_pid : int
    ; ts_id : int
    ; nw_id : int
    ; orig_nw_id : int
    ; nw_name : string
    ; bouquet_name : string
    } [@@deriving yojson, eq]

end

module Pid = struct

  type t = id * info
  and id = int
  and typ =
    | SEC of int list
    | PES of pes
    | ECM of ecm
    | EMM of emm
    | Private
    | Null
  and emm =
    { ca_sys_id : int
    }
  and ecm = emm
  and pes =
    { stream_type : int
    ; stream_id : int
    }
  and info =
    { has_pts : bool
    ; has_pcr : bool
    ; scrambled : bool
    ; present : bool
    ; service_id : int option
    ; service_name : string option [@default None]
    ; typ : typ [@key "type"]
    } [@@deriving yojson, eq, show, ord]

  let typ_to_string : typ -> string = function
    | SEC l ->
       let s = List.map CCFun.(Mpeg_ts.(table_to_string % table_of_int)) l
               |> String.concat ", " in
       "SEC -> " ^ s
    | PES x ->
       let s = Mpeg_ts.stream_type_to_string x.stream_type in
       "PES -> " ^ s
    | ECM x -> "ECM -> " ^ (string_of_int x.ca_sys_id)
    | EMM x -> "EMM -> " ^ (string_of_int x.ca_sys_id)
    | Null -> "Null"
    | Private -> "Private"

end

module Service = struct

  type element = int * Pid.typ [@@deriving yojson, eq]

  type t = id * info
  and id = int
  and info =
    { name : string
    ; provider_name : string
    ; pmt_pid : int
    ; pcr_pid : int
    ; has_pmt : bool
    ; has_sdt : bool
    ; dscr : bool
    ; dscr_list : bool
    ; eit_schedule : bool
    ; eit_pf : bool
    ; free_ca_mode : bool
    ; running_status : int
    ; service_type : int
    ; service_type_list : int
    ; elements : int list
    } [@@deriving yojson, eq, show]

end

module SI_PSI_section = struct

  type t = id * info
  and id =
    { table_id : int
    ; table_id_ext : int
    ; id_ext_1 : int (* For SDT - orig nw id, for EIT - ts id *)
    ; id_ext_2 : int (* For EIT - orig nw id *)
    ; section : int
    }
  and info =
    { pid : int
    ; version : int
    ; service_id : int option
    ; service_name : string option [@default None]
    ; eit_segment_lsn : int
    ; eit_last_table_id : int
    ; section_syntax : bool
    ; last_section : int
    ; length : int
    } [@@deriving yojson, show, eq, ord]

  module Dump = struct

    type parsed = node list
    and node =
      { offset : int
      ; length : int
      ; name : string
	    ; value : value * string option
      }
    and integer =
      | Bool of bool
      | Int of int
      | Int32 of int32
      | Int64 of int64
      | Uint of int
      | Uint32 of int32
      | Uint64 of int64
    and value =
      | List of node list
      | Bytes of int list
      | String of string
      | Bits of integer
      | Dec of integer
      | Hex of integer
      | Time of Time.t
      | Duration of Time.Period.t [@@deriving yojson, show]

    type error =
      | Zero_length
      | Table_not_found
      | Section_not_found
      | Stream_not_found
      | Unknown [@@deriving yojson]

    type t =
      { stream_id : Stream.Multi_TS_ID.t
      ; table_id : int
      ; section_id : int
      ; section : int list
      ; content : parsed option
      } [@@deriving yojson]

  end

end

module SI_PSI_table = struct

  type t = id * info
  and id =
    { table_id : int
    ; table_id_ext : int
    ; id_ext_1 : int (* See SI_PSI_section.id *)
    ; id_ext_2 : int (* See SI_PSI_section.id *)
    }
  and section_info =
    { section : int
    ; length : int
    }
  and info =
    { pid : int
    ; version : int
    ; service_id : int option
    ; service_name : string option [@default None]
    ; section_syntax : bool
    ; last_section : int
    ; eit_segment_lsn : int
    ; eit_last_table_id : int
    ; sections : section_info list
    } [@@deriving yojson, show, eq, ord]

end

module T2mi_info = struct

  type l1_pre =
    { typ : int
    ; preamble : int
    ; fft : int
    ; mixed_flag : bool
    ; bwt_ext : bool
    ; s1 : int
    ; s2 : int
    ; l1_repetition_flag : bool
    ; guard_interval : int
    ; papr : int
    ; l1_mod : int
    ; l1_cod : int
    ; l1_fec_type : int
    ; l1_post_size : int
    ; l1_post_info_size : int
    ; pilot_pattern : int
    ; tx_id_availability : int
    ; cell_id : int
    ; network_id : int
    ; t2_system_id : int
    ; num_t2_frames : int
    ; num_data_symbols : int
    ; regen_flag : int
    ; l1_post_extension : bool
    ; num_rf : int
    ; current_rf_idx : int
    ; t2_version : int
    ; l1_post_scrambled : bool
    ; t2_base_lite : bool
    ; reserved : int
    } [@@deriving yojson, show, eq]

  type l1_post_conf =
    { sub_slices_per_frame : int
    ; aux_config_rfu : int
    ; rf : t2_l1_post_conf_rf list
    ; fef : t2_l1_post_conf_fef option
    ; plp : t2_l1_post_conf_plp list
    ; fef_length_msb : int
    ; reserved_2 : int
    ; aux : t2_l1_post_conf_aux list
    }
  and t2_l1_post_conf_rf =
    { rf_idx : int
    ; frequency : int
    }
  and t2_l1_post_conf_fef =
    { fef_type : int
    ; fef_length : int
    ; fef_interval : int
    }
  and t2_l1_post_conf_plp =
    { plp_id : int
    ; plp_type : int
    ; plp_payload_type : int
    ; ff_flag : bool
    ; first_rf_idx : int
    ; first_frame_idx : int
    ; plp_group_id : int
    ; plp_cod : int
    ; plp_mod : int
    ; plp_rotation : bool
    ; plp_fec_type : int
    ; plp_num_blocks_max : int
    ; frame_interval : int
    ; time_il_length : int
    ; time_il_type : bool
    ; in_band_a_flag : bool
    ; in_band_b_flag : bool
    ; reserved_1 : int
    ; plp_mode : int
    ; static_flag : bool
    ; static_padding_flag : bool
    }
  and t2_l1_post_conf_aux =
    { aux_stream_type : int
    ; aux_private_conf : int
    } [@@deriving yojson, show, eq]

  type l1 =
    { l1_pre : l1_pre
    ; l1_post_conf : l1_post_conf
    } [@@deriving yojson, show, eq]

  type t = id * info
  and id = int
  and info =
    { packets : int list
    ; t2mi_pid : int option
    ; l1 : l1 option
    ; l1_empty : bool [@default false]
    ; l1_parse_error : bool [@default false]
    } [@@deriving yojson, show, eq]

end

module T2mi_sequence = struct

  (** T2-MI packet sequence *)

  (* T2-MI packet types:
   * 0x00  - BB frame
   * 0x01  - Auxiliary stream I/Q data
   * 0x02  - Arbitrary cell insertion
   * 0x10  - L1 current
   * 0x11  - L1 future
   * 0x12  - P2 bias balancing cells
   * 0x20  - DVB-T2 timestamp
   * 0x21  - Individual addressing
   * 0x30  - FEF part : Null
   * 0x31  - FEF part : I/Q data
   * 0x32  - FEF part : composite
   * 0x33  - FEF sub-part
   * other - Reserved for future use
   *)

  (* NOTE suggest using fields like plp, frame,
   * l1_param_1 and l1_param_2 as abstract parameters,
   * which have its own meaning for each packet type
   *)

  type item =
    { typ : int (* T2-MI packet type according to TS 102 773 *)
    ; super_frame : int
    ; stream_id : int
    ; frame : int (* for packet types 0x00 .. 0x02, 0x10 .. 0x12 *)
    ; count : int
    ; plp : int (* only for BB frames *)
    ; l1_param_1 : int (* L1DYN_CURR.FRAME_IDX  for L1 current,
                        * L1DYN_NEXT.FRAME_IDX for L1 future *)
    ; l1_param_2 : int (* L1DYN_NEXT2.FRAME_IDX for L1 future *)
    ; ts_packet : int
    } [@@deriving yojson]

  type t = item list [@@deriving yojson]

end

module Streams = struct

  type streams_states =
    (Stream.t timespan) list [@@deriving yojson]

  type streams_unique =
    (Stream.t * [`Now | `Last of Time.t]) list [@@deriving yojson]

end

module Error = struct

  type segmentation =
    { errors : float
    ; no_stream : float
    ; no_measure : float
    } [@@deriving yojson]

  type 'a error =
    { count : int
    ; err_code : int
    ; err_ext : int
    ; priority : int
    ; multi_pid : bool
    ; pid : 'a
    ; packet : int32
    ; service_id : int option
    ; service_name : string option [@default None]
    ; param_1 : int32
    ; param_2 : int32 (* t2mi stream id for t2mi error *)
    ; time : Time.t
    }
  and t = int error [@@deriving yojson, eq, show]
  and t_ext = (Pid.id * Pid.info option) error

  type compressed = percent timespan list
  and percent =
    { errors : float
    ; no_stream : float
    } [@@deriving yojson]

end

(* Helper types *)

type bitrates =
  (Stream.ID.t * Bitrate.t timestamped) list [@@deriving yojson, eq]
type ts_info =
  (Stream.ID.t * Ts_info.t timestamped) list [@@deriving yojson, eq]
type pids =
  (Stream.ID.t * Pid.t list timestamped) list [@@deriving yojson, eq]
type services =
  (Stream.ID.t * (Service.t list timestamped)) list [@@deriving yojson, eq]
type elements =
  (Stream.ID.t * ((int * int) * Pid.typ) list timestamped) list [@@deriving yojson, eq]
type tables =
  (Stream.ID.t * (SI_PSI_table.t list timestamped)) list [@@deriving yojson, eq]
type sections =
  (Stream.ID.t * (SI_PSI_section.t list timestamped)) list [@@deriving yojson, eq]
type t2mi_info =
  (Stream.ID.t * T2mi_info.t list timestamped) list [@@deriving yojson, eq]
type errors =
  (Stream.ID.t * (Error.t list)) list [@@deriving yojson, eq]
