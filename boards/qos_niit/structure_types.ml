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

type table_label = [`PAT   | `CAT   | `PMT   | `TSDT  |
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
