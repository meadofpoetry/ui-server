open Common
open Storage.Database

module R = Caqti_request

module ID : sig

  type t = Stream.ID.t
  type db
  val db : db Caqti_type.t
  val typ : string
  val of_db : db -> t
  val to_db : t -> db
  val to_value_string : t -> string

end = struct

  type t = Stream.ID.t
  type db = string

  let typ : string = "UUID"
  let db : db Caqti_type.t = Types.string
  let to_db (id : t) : db =
    Stream.ID.to_string id
  let of_db (db : db) : t =
    Stream.ID.of_string db
  let to_value_string x =
    let s = Stream.ID.to_string x in
    Printf.sprintf "'%s'::%s" s typ

end

module Model = struct
  open Key_t

  type init = int
  type names =
    { state : string
    ; streams : string
    ; ts_info : string
    ; services : string
    ; si_psi_sections : string
    ; pids : string
    ; t2mi_info : string
    ; bitrate : string
    ; pids_bitrate : string
    ; errors : string
    }

  let name = "qos_niit"

  let keys_state =
    make_keys ~time_key:"date_end"
      [ "state", key "INTEGER"
      ; "date_start", key "TIMESTAMP"
      ; "date_end", key "TIMESTAMP"
      ]

  let keys_streams =
    make_keys ~time_key:"date_end"
      [ "stream", key "JSONB"
      ; "id", key ID.typ
      ; "incoming", key "BOOL"
      ; "type", key "TEXT"
      ; "input", key "JSONB"
      ; "date_start", key "TIMESTAMP"
      ; "date_end", key "TIMESTAMP"
      ]

  let keys_ts_info =
    make_keys ~time_key:"date_end"
      [ "stream", key ~primary:true ID.typ
      
      ; "complete", key "BOOL"
      ; "services", key "INTEGER"
      ; "nw_pid", key "INTEGER"
      ; "ts_id", key "INTEGER"
      ; "nw_id", key "INTEGER"
      ; "orig_nw_id", key "INTEGER"
      ; "nw_name", key "TEXT"
      ; "bouquet_name", key "TEXT"
      ; "date_start", key ~primary:true "TIMESTAMP"
      ; "date_end", key "TIMESTAMP"
      ]

  let keys_services =
    make_keys ~time_key:"date_end"
      [ "stream", key ~primary:true ID.typ
      ; "id", key ~primary:true "INTEGER"
      ; "name", key "TEXT"
      ; "provider", key "TEXT"
      ; "pmt_pid", key "INTEGER"
      ; "pcr_pid", key "INTEGER"
      ; "has_pmt", key "BOOL"
      ; "has_sdt", key "BOOL"
      ; "dscr", key "BOOL"
      ; "dscr_list", key "BOOL"
      ; "eit_schedule", key "BOOL"
      ; "eit_pf", key "BOOL"
      ; "free_ca_mode", key "BOOL"
      ; "running_status", key "INTEGER"
      ; "service_type", key "INTEGER"
      ; "service_type_list", key "INTEGER"
      ; "elements", key "JSONB"
      ; "date_start", key ~primary:true "TIMESTAMP"
      ; "date_end", key "TIMESTAMP"
      ]

  (* FIXME a lot of repetitive data *)
  let keys_si_psi_sections =
    make_keys ~time_key:"date_end"
      [ "stream", key ~primary:true ID.typ
      ; "table_id", key ~primary:true "INTEGER"
      ; "table_id_ext", key ~primary:true "INTEGER"
      ; "id_ext_1", key ~primary:true "INTEGER"
      ; "id_ext_2", key ~primary:true "INTEGER"
      ; "section", key ~primary:true "INTEGER"
      ; "pid", key "INTEGER"
      ; "version", key "INTEGER"
      ; "service_id", key "INTEGER"
      ; "section_syntax", key "BOOL"
      ; "last_section", key "INTEGER"
      ; "length", key "INTEGER"
      ; "date_start", key ~primary:true "TIMESTAMP"
      ; "date_end", key "TIMESTAMP"
      ]

  let keys_pids =
    make_keys ~time_key:"date_end"
      [ "stream", key ~primary:true ID.typ
      ; "pid", key ~primary:true "INTEGER"
      ; "service", key "TEXT"
      ; "type", key "JSONB"
      ; "has_pts", key "BOOL"
      ; "has_pcr", key "BOOL"
      ; "scrambled", key "BOOL"
      ; "present", key "BOOL"
      ; "date_start", key ~primary:true "TIMESTAMP"
      ; "date_end", key "TIMESTAMP"
      ]

  let keys_t2mi_info =
    make_keys ~time_key:"date_end"
      [ "stream", key ~primary:true ID.typ
      ; "t2mi_pid", key ~primary:true "INTEGER"
      ; "t2mi_stream_id", key ~primary:true "INTEGER"
      ; "packets", key "JSONB"
      ; "l1_pre", key "JSONB"
      ; "l1_post_conf", key "JSONB"
      ; "l1_empty", key "BOOL"
      ; "l1_parse_error", key "BOOL"
      ; "date_start", key ~primary:true "TIMESTAMP"
      ; "date_end", key "TIMESTAMP"
      ]

  let keys_bitrate =
    make_keys ~time_key:"date"
      [ "stream", key ~primary:true ID.typ
      ; "bitrate", key "INTEGER"
      ; "date", key ~primary:true "TIMESTAMP"
      ]

  let keys_pids_bitrate =
    make_keys ~time_key:"date"
      [ "stream", key ~primary:true ID.typ
      ; "pid", key ~primary:true "INTEGER"
      ; "bitrate", key "INTEGER"
      ; "date", key ~primary:true "TIMESTAMP"
      ]

  let keys_errors =
    make_keys ~time_key:"date"
      [ "is_ts",     key "BOOL"
      ; "stream",    key ID.typ
      ; "count",     key "INTEGER"
      ; "err_code",  key "INTEGER"
      ; "err_ext",   key "INTEGER"
      ; "priority",  key "INTEGER"
      ; "multi_pid", key "BOOL"
      ; "pid",       key "INTEGER"
      ; "packet",    key "INTEGER"
      ; "param_1",   key "INTEGER"
      ; "param_2",   key "INTEGER"
      ; "date",      key "TIMESTAMP"
      ]

  let tables id =
    let id = string_of_int id in
    let names =
      { state = "qos_niit_state_" ^ id
      ; streams = "qos_niit_streams_" ^ id
      ; ts_info = "qos_niit_ts_info_" ^ id
      ; services = "qos_niit_services_" ^ id
      ; si_psi_sections = "qos_niit_si_psi_sections_" ^ id
      ; pids = "qos_niit_pids_" ^ id
      ; t2mi_info = "qos_niit_t2mi_info_" ^ id
      ; bitrate = "qos_niit_bitrate_" ^ id
      ; pids_bitrate = "qos_niit_pids_bitrate_" ^ id
      ; errors = "qos_niit_errors_" ^ id
      }
    in
    names,
    [ names.state, keys_state, None
    ; names.streams, keys_streams, None
    ; names.ts_info, keys_ts_info, None
    ; names.services, keys_services, None
    ; names.si_psi_sections, keys_si_psi_sections, None
    ; names.pids, keys_pids, None
    ; names.t2mi_info, keys_t2mi_info, None
    ; names.bitrate, keys_bitrate, None
    ; names.pids_bitrate, keys_pids_bitrate, None
    ; names.errors, keys_errors, None
    ]

end

module Conn = Storage.Database.Make(Model)

type t = Conn.t

let to_columns_string (keys : keys) =
  let cols = keys.columns in
  String.concat "," @@ List.map fst cols

let to_values (keys : keys) =
  List.map (fun _ -> "?") keys.columns
  |> String.concat ","

let unwrap = function Ok v -> v | Error e -> failwith e

let is_in field to_string = function
  | [] -> ""
  | lst -> Printf.sprintf " %s IN (%s) AND "
             field (String.concat "," @@ List.map to_string lst)
