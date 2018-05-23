open Common
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
  ; stream         : Stream.id (* NOTE maybe t? *)
  } [@@deriving yojson,eq]

type jitter_mode =
  { stream  : Stream.id (* NOTE maybe t? *)
  ; pid     : int
  } [@@deriving yojson,eq]

type t2mi_mode_request   = t2mi_mode option   [@@deriving yojson]
type jitter_mode_request = jitter_mode option [@@deriving yojson]

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
  ; stream_id   : int
  ; frame       : int (* for packet types 0x00 .. 0x02, 0x10 .. 0x12 *)
  ; count       : int
  ; plp         : int (* only for BB frames *)
  ; l1_param_1  : int (* L1DYN_CURR.FRAME_IDX  for L1 current, L1DYN_NEXT.FRAME_IDX for L1 future *)
  ; l1_param_2  : int (* L1DYN_NEXT2.FRAME_IDX for L1 future *)
  ; ts_packet   : int
  } [@@deriving yojson]

type t2mi_packets = t2mi_packet list [@@deriving yojson]

(** SI/PSI section **)

type section_request =
  { stream_id      : Stream.id
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

type section =
  { stream_id : Stream.id
  ; table_id  : int
  ; section   : string } [@@deriving yojson]

(** Config **)

type config =
  { input       : input
  ; t2mi_mode   : t2mi_mode option
  ; jitter_mode : jitter_mode option
  } [@@deriving yojson,eq]

let config_to_string c = Yojson.Safe.to_string @@ config_to_yojson c
let config_of_string s = config_of_yojson @@ Yojson.Safe.from_string s

let config_default =
  { input       = ASI
  ; t2mi_mode   = None
  ; jitter_mode = None
  }

type packet_sz = Ts188 | Ts192 | Ts204 [@@deriving eq]
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
  { timestamp    : Time.t
  ; load         : float
  ; ts_num       : int
  ; services_num : int
  ; bitrate      : int
  ; packet_sz    : packet_sz
  ; has_sync     : bool
  ; has_stream   : bool
  } [@@deriving yojson,eq]

type reset_ts =
  { timestamp : Time.t
  }

type statuses = status list [@@deriving yojson]

(** Board errors **)

type board_error =
  { timestamp : Time.t
  ; err_code  : int
  ; count     : int
  } [@@deriving yojson]

type board_errors = board_error list [@@deriving yojson]

module Jitter = struct

  type session =
    { timestamp : Time.t
    ; t_pcr     : float
    ; mode      : jitter_mode
    } [@@deriving yojson,eq]

  type measure =
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

  type measures = measure list [@@deriving yojson]

  type archive_item =
    { session  : session
    ; measures : measure list
    } [@@deriving yojson]

  type archive = archive_item list [@@deriving yojson]

end

module Streams = struct

  module TS = struct

    type state =
      { stream    : Stream.id
      ; timestamp : Time.t
      ; present   : bool
      } [@@deriving yojson]

    type states = state list [@@deriving yojson]

    type pid_info =
      { pid       : int
      ; bitrate   : int option
      ; has_pts   : bool
      ; scrambled : bool
      ; present   : bool
      } [@@deriving yojson]

    type es_info =
      { pid          : int
      ; bitrate      : int option
      ; has_pts      : bool
      ; es_type      : int
      ; es_stream_id : int
      } [@@deriving yojson]

    type ecm_info =
      { pid       : int
      ; bitrate   : int option
      ; ca_sys_id : int
      } [@@deriving yojson]

    type service_info =
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
      ; es             : es_info list
      ; ecm            : ecm_info list
      } [@@deriving yojson]

    type emm_info = ecm_info [@@deriving yojson]

    type actual_other = Actual  | Other    [@@deriving yojson]
    type eit_type     = Present | Schedule [@@deriving yojson]

    type section_info =
      { id       : int
      ; analyzed : bool
      ; length   : int
      } [@@deriving yojson]

    type table_info =
      { version        : int
      ; bitrate        : int option
      ; id             : int
      ; pid            : int
      ; lsn            : int
      ; section_syntax : bool
      ; sections       : section_info list
      } [@@deriving yojson]

    type pat_info =
      { common : table_info
      ; ts_id  : int (* table id ext *)
      } [@@deriving yojson]

    type pmt_info =
      { common         : table_info
      ; program_number : int (* table id ext *)
      } [@@deriving yojson]

    type nit_info =
      { common : table_info
      ; ts     : actual_other
      ; nw_id  : int (* table id ext *)
      } [@@deriving yojson]

    type sdt_info =
      { common : table_info
      ; ts_id  : int (* table id ext *)
      ; ts     : actual_other
      } [@@deriving yojson]

    type bat_info =
      { common     : table_info
      ; bouquet_id : int (* table id ext *)
      } [@@deriving yojson]

    type eit_params =
      { ts_id         : int (* eit param 1*)
      ; orig_nw_id    : int (* eit param 2*)
      ; segment_lsn   : int
      ; last_table_id : int
      } [@@deriving yojson]

    type eit_info =
      { common     : table_info
      ; service_id : int (* table id ext *)
      ; ts         : actual_other
      ; typ        : eit_type
      ; params     : eit_params
      } [@@deriving yojson]

    type table = PAT     of pat_info
               | CAT     of table_info
               | PMT     of pmt_info
               | TSDT    of table_info
               | NIT     of nit_info
               | SDT     of sdt_info
               | BAT     of bat_info
               | EIT     of eit_info
               | TDT     of table_info
               | RST     of table_info
               | ST      of table_info
               | TOT     of table_info
               | DIT     of table_info
               | SIT     of table_info
               | Unknown of table_info [@@deriving yojson]

    type general_info =
      { complete     : bool
      ; services_num : int
      ; nw_pid       : int
      ; ts_id        : int
      ; nw_id        : int
      ; orig_nw_id   : int
      ; nw_name      : string
      ; bouquet_name : string
      } [@@deriving yojson]

    type structure =
      { timestamp : Time.t
      ; stream    : Stream.id
      ; bitrate   : int option
      ; general   : general_info
      ; pids      : pid_info list
      ; services  : service_info list
      ; emm       : emm_info list
      ; tables    : table list
      } [@@deriving yojson]

    type structures         = structure list [@@deriving yojson]
    type structure_response = structure option [@@deriving yojson]

    type table_label = [ `PAT   | `CAT   | `PMT   | `TSDT  |
                         `NIT   | `NITa  | `NITo  |
                         `SDT   | `SDTa  | `SDTo  | `BAT   |
                         `EIT   | `EITap | `EITop | `EITas | `EITos |
                         `TDT   | `RST   | `ST    | `TOT   | `DIT   | `SIT   |
                         `Unknown of int
                       ]

    let table_label_of_int : int -> table_label = function
      | 0x00 -> `PAT
      | 0x01 -> `CAT
      | 0x02 -> `PMT
      | 0x03 -> `TSDT
      | 0x40 -> `NITa
      | 0x41 -> `NITo
      | 0x42 -> `SDTa
      | 0x46 -> `SDTo
      | 0x4A -> `BAT
      | 0x4E -> `EITap
      | 0x4F -> `EITop
      | x when x >= 0x50 && x <= 0x5F -> `EITas
      | x when x >= 0x60 && x <= 0x6F -> `EITos
      | 0x70 -> `TDT
      | 0x71 -> `RST
      | 0x72 -> `ST
      | 0x73 -> `TOT
      | 0x7E -> `DIT
      | 0x7F -> `SIT
      | x    -> `Unknown x

    let table_label_to_string : table_label -> string = function
      | `PAT   -> "PAT"
      | `CAT   -> "CAT"
      | `PMT   -> "PMT"
      | `TSDT  -> "TSDT"
      | `NIT   -> "NIT"
      | `NITa  -> "NIT actual"
      | `NITo  -> "NIT other"
      | `SDT   -> "SDT"
      | `SDTa  -> "SDT actual"
      | `SDTo  -> "SDT other"
      | `BAT   -> "BAT"
      | `EIT   -> "EIT"
      | `EITap -> "EIT actual present"
      | `EITop -> "EIT other present"
      | `EITas -> "EIT actual schedule"
      | `EITos -> "EIT other schedule"
      | `TDT   -> "TDT"
      | `RST   -> "RST"
      | `ST    -> "ST"
      | `TOT   -> "TOT"
      | `DIT   -> "DIT"
      | `SIT   -> "SIT"
      | `Unknown _ -> "Unknown"

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

    let table_common_of_table = function
      | PAT x     -> x.common | CAT x     -> x        | PMT x     -> x.common
      | TSDT x    -> x        | NIT x     -> x.common | SDT x     -> x.common
      | BAT x     -> x.common | EIT x     -> x.common | TDT x     -> x
      | RST x     -> x        | ST  x     -> x        | TOT x     -> x
      | DIT x     -> x        | SIT x     -> x        | Unknown x -> x

  end

  module T2MI = struct

    type state =
      { stream    : Stream.id
      ; timestamp : Time.t
      ; stream_id : int
      ; present   : bool
      } [@@deriving yojson]

    type states = state list [@@deriving yojson]

    type structure =
      { packets      : int list
      ; stream_id    : int
      ; t2mi_pid     : int option
      ; l1_pre       : string option
      ; l1_post_conf : string option
      } [@@deriving yojson]

    type structures         = structure list [@@deriving yojson]
    type structure_response = structure option [@@deriving yojson]

  end

end

module Errors = struct

  open Common.Time

  type 'a _archive_response =
    { errors     : 'a list
    ; no_sync    : Interval.t list
    ; no_measure : Interval.t list
    } [@@deriving yojson]

  type segmentation =
    { errors     : float
    ; no_stream  : float
    ; no_measure : float
    } [@@deriving yojson]

  module TS = struct

    type t =
      { stream    : Stream.id
      ; timestamp : Time.t
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

    type t_list = t list [@@deriving yojson]

    type archive_request =
      { priority : int option
      ; errors   : int list option
      ; period   : Interval.t
      ; stream   : Stream.id option
      } [@@deriving yojson]
    type archive_response = t _archive_response [@@deriving yojson]

  end

  module T2MI = struct

    type t =
      { stream    : Stream.id
      ; timestamp : Time.t
      ; stream_id : int
      ; pid       : int
      ; err_code  : int
      ; sync      : bool
      ; count     : int
      ; param     : int option
      } [@@deriving yojson]

    type t_list = t list [@@deriving yojson]

    type archive_request =
      { errors : int list option
      ; period : Interval.t
      ; stream : Stream.id option
      } [@@deriving yojson]
    type archive_response = t _archive_response [@@deriving yojson]

  end

end

module Path = struct

  type item    = string
  type t       = item list
  type req_typ = [ `WS  | `REST    ]
  type archive = [ `Now | `Archive ]

  module Infix = struct

    let (^::) = List.cons_maybe
    let (@)   = List.(@)

  end

  open Infix

  let to_string (t:t) = List.fold_left Filename.concat "" t

  let make ?(archive)
           ?(api)
           ?(subdomains=[])
           ?(parameters=[])
           (domain:item) =
    let ws   = Option.map (function `REST -> "rest" | `WS -> "ws") api in
    let arch = Option.map (function `Archive -> "archive" | `Now -> "now") archive in
    (List.rev parameters) @ (arch ^:: ws ^:: (List.rev subdomains @ [domain]))
    |> List.rev

  let full control t =
    let prefix = ["api"; "board"; string_of_int control ] in
    (prefix @ t)

end

module Request = struct

  open Path

  let ws = "ws"

  type board_mode =
    | T2MI
    | Jitter
  type board_set =
    | Reset
    | Mode of board_mode
  type board_get_rt =
    | Devinfo
    | Errors  of req_typ
    | Config  of req_typ
    | Status
  type board_get_ar =
    | Errors
    | Status
  type board =
    | Set    of board_set
    | Get_rt of board_get_rt
    | Get_ar of board_get_ar

  let board_mode_to_string = function T2MI -> "t2mi" | Jitter -> "jitter"
  let board_mode_of_string = function "t2mi" -> Some T2MI | "jitter" -> Some Jitter | _ -> None

  let board_to_path : board -> Path.t = function
    | Set x    -> (match x with
                   | Reset  -> make "reset"
                   | Mode x -> let subdomains = List.return @@ board_mode_to_string x in
                               make ~subdomains "mode")
    | Get_rt x -> (match x with
                   | Devinfo  -> make "devinfo"
                   | Config x -> make "config"
                   | Errors x -> make ~api:x   ~archive:`Now "errors"
                   | Status   -> make ~api:`WS ~archive:`Now "status")
    | Get_ar x -> (match x with
                   | Errors   -> make ~api:`REST ~archive:`Archive "errors"
                   | Status   -> make ~api:`REST ~archive:`Archive "status")

  let board_of_path : Path.t -> board option = function
    | ["reset"]  -> Some (Set Reset)
    | ["mode";s] -> Option.map (fun m -> Set (Mode m)) @@ board_mode_of_string s
    | [""]
      | _          -> None

  type t =
    | Board of board

  let to_path : t -> Path.t = function
    | Board x -> board_to_path x

  let of_path : Path.t -> t option = function
    | _ -> None

end

