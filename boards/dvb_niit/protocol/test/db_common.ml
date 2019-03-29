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
    { measurements : string
    }

  let name = "dvb_niit"

  let keys_measurements =
    make_keys ~time_key:"date"
      [ "tuner", key ~primary:true "INTEGER"
      ; "stream", key ~primary:true ID.typ
      ; "lock", key "BOOL"
      ; "power", key "REAL"
      ; "mer", key "REAL"
      ; "ber", key "REAL"
      ; "freq", key "INTEGER"
      ; "bitrate", key "INTEGER"
      ; "date", key ~primary:true "TIMESTAMP"
      ]

  let tables id =
    let id = string_of_int id in
    let names =
      { measurements = "dvb_niit_measurements_" ^ id
      } in
    names,
    [names.measurements, keys_measurements, None]

end

module Conn = Storage.Database.Make(Model)

let is_in field to_string = function
  | [] -> ""
  | lst -> Printf.sprintf " %s IN (%s) AND "
             field (String.concat "," @@ List.map to_string lst)
