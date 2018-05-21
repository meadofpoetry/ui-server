open Common.Dvb_t2_types
open Common.Topology
open Containers

(** Board info **)

type devinfo =
  { typ : int
  ; ver : int
  } [@@deriving yojson]

type devinfo_response = devinfo option [@@deriving yojson]

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
  } [@@deriving yojson,eq]

type t2mi_mode_request   = t2mi_mode option   [@@deriving yojson]
type jitter_mode_request = jitter_mode option [@@deriving yojson]

(** Status **)

type packet_sz = Ts188 | Ts192 | Ts204
let packet_sz_to_string : packet_sz -> string = function
  | Ts188 -> "Ts188"
  | Ts192 -> "Ts192"
  | Ts204 -> "Ts204"
let packet_sz_of_string_option : string -> packet_sz option = function
  | "Ts188" -> Some Ts188
  | "Ts192" -> Some Ts192
  | "Ts204" -> Some Ts204
  | _       -> None
let packet_sz_to_yojson x : Yojson.Safe.json = `String (packet_sz_to_string x)
let packet_sz_of_yojson = function
  | `String s -> (match packet_sz_of_string_option s with
                  | Some x -> Ok x
                  | None   -> Error (Printf.sprintf "packet_sz_of_yojson: bad string (%s)" s))
  | x         -> Error (Printf.sprintf "packet_sz_of_yojson: not string value (%s)" @@ Yojson.Safe.to_string x)

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
  } [@@deriving yojson]

(** MPEG-TS errors **)

type ts_sync =
  { stream_id : Common.Stream.id
  ; timestamp : Common.Time.t
  ; sync      : bool
  }

type ts_error =
  { stream_id : Common.Stream.id
  ; timestamp : Common.Time.t
  ; count     : int
  ; err_code  : int
  ; err_ext   : int
  ; priority  : int
  ; multi_pid : bool
  ; pid       : int
  ; packet    : int32
  ; param_1   : int32
  ; param_2   : int32
  } [@@deriving yojson]

type ts_errors = ts_error list [@@deriving yojson]

(** T2-MI errors **)

type t2mi_sync =
  { stream_id      : Common.Stream.id
  ; timestamp      : Common.Time.t
  ; t2mi_stream_id : int
  ; sync           : bool
  }

type t2mi_error =
  { stream_id      : Common.Stream.id
  ; timestamp      : Common.Time.t
  ; t2mi_stream_id : int
  ; pid            : int
  ; err_code       : int
  ; sync           : bool
  ; count          : int
  ; param          : int
  } [@@deriving yojson]

type t2mi_errors = t2mi_error list [@@deriving yojson]

(** Board errors **)

type board_error =
  { timestamp : Common.Time.t
  ; err_code  : int
  ; count     : int
  } [@@deriving yojson]

type board_errors = board_error list [@@deriving yojson]

(** T2-MI frames sequence **)

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

(* NOTE suggest using fields like plp, frame, l1_param_1 and l1_param_2 as abstract parameters,
 * which have its own meaning for each packet type
 *)

type t2mi_packet =
  { typ         : int (* T2-MI packet type according to TS 102 773 *)
  ; super_frame : int
  ; frame       : int (* for packet types 0x00 .. 0x02, 0x10 .. 0x12 *)
  ; count       : int
  ; plp         : int (* only for BB frames *)
  ; l1_param_1  : int (* L1DYN_CURR.FRAME_IDX  for L1 current, L1DYN_NEXT.FRAME_IDX for L1 future *)
  ; l1_param_2  : int (* L1DYN_NEXT2.FRAME_IDX for L1 future *)
  ; ts_packet   : int
  } [@@deriving yojson]

type t2mi_packets = t2mi_packet list [@@deriving yojson]

(** Jitter **)

type jitter_session =
  { timestamp : Common.Time.t
  ; t_pcr     : float
  ; mode      : jitter_mode
  } [@@deriving yojson,eq]

type jitter_measure =
  { discont_err : bool
  ; discont_ok  : bool
  ; t_pcr       : float
  (* for charts *)
  ; accuracy    : float
  ; jitter      : int
  ; drift       : float
  ; fo          : float
  ; period      : float
  } [@@deriving yojson]

type jitter_measures = jitter_measure list [@@deriving yojson]

(** SI/PSI section **)

type section_request =
  { stream_id      : Common.Stream.id
  ; table_id       : int
  ; section        : int option (* needed for tables containing multiple sections *)
  ; table_id_ext   : int option (* needed for tables with extra parameter, like ts id for PAT *)
  ; eit_ts_id      : int option (* ts id for EIT *)
  ; eit_orig_nw_id : int option (* original network ID for EIT *)
  } [@@deriving yojson]

type section_error = Zero_length
                   | Table_not_found
                   | Section_not_found
                   | Stream_not_found
                   | Unknown [@@deriving yojson]

type section = string [@@deriving yojson]

(** T2-MI info **)

type t2mi_info =
  { packets        : int list
  ; t2mi_stream_id : int
  ; t2mi_pid       : int option
  ; l1_pre         : string option
  ; l1_post_conf   : string option
  } [@@deriving yojson]

(** Config **)

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

(** Measurements requests **)

module TS_errors_api = struct

  open Common.Time

  type percentage =
    { errors  : float
    ; no_sync : float
    ; no_data : float
    } [@@deriving yojson]

  type ts_errors_response_archive =
    { errors  : ts_errors list
    ; no_data : Interval.Seconds.t list
    ; no_sync : Interval.Seconds.t list
    } [@@deriving yojson]

end
