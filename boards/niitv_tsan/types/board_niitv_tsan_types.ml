open Application_types

let ( % ) f g x = f (g x)

type 'a ts =
  { data : 'a
  ; timestamp : Time.t
  } [@@deriving yojson, eq, show]

let stamp (data : 'a) (timestamp : Time.t) =
  { data; timestamp }

type 'a tspan =
  { data : 'a
  ; from : Time.t
  ; till : Time.t
  } [@@deriving yojson, eq, show]

type devinfo =
  { typ : int
  ; ver : int
  } [@@deriving yojson, eq]

let pp_devinfo ppf di =
  Format.fprintf ppf "type: 0x%02X, version: %d" di.typ di.ver

let show_devinfo = Format.asprintf "%a" pp_devinfo

type input =
  | SPI
  | ASI [@@deriving enum, eq]

let pp_input ppf = function
  | SPI -> Format.pp_print_string ppf "SPI"
  | ASI -> Format.pp_print_string ppf "ASI"

let show_input = Format.asprintf "%a" pp_input

let input_to_yojson = Util_json.Int.to_yojson % input_to_enum

let input_of_yojson json =
  match Util_json.Int.of_yojson json with
  | Error _ as e -> e
  | Ok i ->
    match input_of_enum i with
    | None -> Error (Printf.sprintf "input_of_yojson: invalid int value (%d)" i)
    | Some x -> Ok x

type stream =
  | ID of Stream.Multi_TS_ID.t
  | Full of Stream.t [@@deriving eq]

let pp_stream ppf = function
  | ID x -> Stream.Multi_TS_ID.pp ppf x
  | Full x -> Stream.pp_container_id ppf x.orig_id

let show_stream s =
  Format.asprintf "%a" pp_stream s

let stream_to_yojson = function
  | ID x -> Stream.Multi_TS_ID.to_yojson x
  | Full x -> Stream.to_yojson x

let stream_of_yojson json =
  match Stream.Multi_TS_ID.of_yojson json with
  | Ok x -> Ok (ID x)
  | Error _ ->
    match Stream.of_yojson json with
    | Ok x -> Ok (Full x)
    | Error _ -> Error "stream_of_yojson: got neither Multi TS ID, nor stream"

type t2mi_mode =
  { enabled : bool
  ; pid : int
  ; t2mi_stream_id : int
  ; stream : stream
  } [@@deriving yojson, show]

let equal_t2mi_mode (a : t2mi_mode as 'a) (b : 'a) =
  a.enabled = b.enabled
  && a.pid = b.pid
  && a.t2mi_stream_id = b.t2mi_stream_id
  && (match a.stream, b.stream with
      | Full x, Full y ->
        Stream.ID.equal x.id y.id
        && Stream.equal_container_id x.orig_id y.orig_id
      | Full x, ID y | ID y, Full x ->
        Stream.equal_container_id x.orig_id (TS_multi y)
      | ID x, ID y -> Stream.Multi_TS_ID.equal x y)

type jitter_mode =
  { pid : int
  ; stream : Stream.Multi_TS_ID.t
  ; stream_id : Stream.ID.t option [@default None]
  } [@@deriving yojson, show]

let equal_jitter_mode (a : jitter_mode as 'a) (b : 'a) =
  a.pid = b.pid
  && Stream.Multi_TS_ID.equal a.stream b.stream
  && (match a.stream_id, b.stream_id with
      | None, Some _ | Some _, None -> false
      | None, None -> true
      | Some a, Some b -> Stream.ID.equal a b)

(** Config *)

type config =
  { input : input
  ; input_source : int
  ; t2mi_source : int
  ; t2mi_mode : t2mi_mode
  ; jitter_mode : jitter_mode
  } [@@deriving yojson, eq]

type packet_sz =
  | Ts188
  | Ts192
  | Ts204 [@@deriving show, eq]

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

module Deverr = struct

  type t =
    { timestamp : Time.t
    ; code : int
    ; count : int
    ; source : source
    ; param : int option
    }
  and source =
    | Hardware
    | Protocol [@@deriving yojson]

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
    ; timestamp : Time.t
    } [@@deriving yojson, eq]


end

module TS_info = struct

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

module PID_info = struct

  type t =
    { has_pts : bool
    ; has_pcr : bool
    ; scrambled : bool
    ; present : bool
    ; service_id : int option
    ; service_name : string option [@default None]
    ; typ : MPEG_TS.PID.Type.t [@key "type"]
    } [@@deriving yojson, eq, show, ord]

end

module Service_info = struct

  type t =
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

  type id =
    { table_id : int
    ; table_id_ext : int
    ; id_ext_1 : int (* For SDT - orig nw id, for EIT - ts id *)
    ; id_ext_2 : int (* For EIT - orig nw id *)
    ; section : int
    } [@@deriving yojson, show, eq, ord]

  type t =
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
      ; section : string
      ; content : Si_psi_parser_types.Node.t list option
      } [@@deriving yojson]

  end

end

module SI_PSI_table = struct

  type id =
    { table_id : int
    ; table_id_ext : int
    ; id_ext_1 : int (* See SI_PSI_section.id *)
    ; id_ext_2 : int (* See SI_PSI_section.id *)
    } [@@deriving yojson, show, eq, ord]

  type section_info =
    { section : int
    ; length : int
    } [@@deriving yojson, show, eq, ord]

  type t =
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

module Structure = struct

  type t =
    { info : TS_info.t
    ; services : (int * Service_info.t) list
    ; tables : (SI_PSI_table.id * SI_PSI_table.t) list
    ; pids : (int * PID_info.t) list
    ; timestamp : Time.t
    } [@@deriving eq, yojson]

  let info (t : t) = t.info

  let pids (t : t) = t.pids

  let services (t : t) = t.services

  let tables (t : t) = t.tables

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

  type t =
    { packets : int list
    ; t2mi_pid : int option
    ; l1 : l1 option
    ; timestamp : Time.t
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
    (Stream.t tspan) list [@@deriving yojson]

  type streams_unique =
    (Stream.t * [`Now | `Last of Time.t]) list [@@deriving yojson]

end

module Error = struct

  type segmentation =
    { errors : float
    ; no_stream : float
    ; no_measure : float
    } [@@deriving yojson]

  type 'a e =
    { count : int
    ; err_code : int
    ; err_ext : int
    ; is_t2mi : bool
    ; multi_pid : bool
    ; pid : 'a
    ; packet : int32
    ; param_1 : int32
    ; param_2 : int32 (* t2mi stream id for t2mi error *)
    ; timestamp : Time.t
    }
  and t = int e [@@deriving yojson, eq, show]

  and t_ext = (int * PID_info.t option) e

  type compressed = percent tspan list
  and percent =
    { errors : float
    ; no_stream : float
    } [@@deriving yojson]

end
