open Containers
open Storage.Database
open Common

open Lwt.Infix

module Model = struct
  let name = "pipeline"
  let table = "qoe_structures"
  let init =
    Exec (Caqti_request.exec Caqti_type.unit
            {eos|CREATE TABLE IF NOT EXISTS qoe_structures(
             input   JSONB,
             structs JSONB,
             date    TIMESTAMP DEFAULT CURRENT_TIMESTAMP
             )|eos})
  let worker = None
end

module Conn = Storage.Database.Make(Model)
    
let insert_structures db streams : unit Lwt.t =
  let entries = Structure_conv.dump_structures streams in
  let insert  = Caqti_request.exec Caqti_type.(tup2 string string)
                  "INSERT INTO qoe_structures(input, structs) VALUES (?,?)"
  in Conn.request db (Trans ((Exec insert),(),(fun x _ -> x))) entries

let select_input db i =
  let i = Yojson.Safe.to_string @@ Common.Topology.topo_input_to_yojson i in
  let unwrap (s,d) =
    (Result.get_exn @@ Structure.Streams.of_yojson @@ Yojson.Safe.from_string s)
    , d
  in
  let get' =
    Caqti_request.find Caqti_type.string Caqti_type.(tup2 string ptime)
      "SELECT structs, date FROM qoe_structures WHERE input = ?::JSONB ORDER BY date DESC LIMIT 1"
  in Conn.request db (Find get') i >|= (Option.map unwrap)

let select_input_between db i from to' =
  let i = Yojson.Safe.to_string @@ Common.Topology.topo_input_to_yojson i in
  let unwrap (s,d) =
    (Result.get_exn @@ Structure.Streams.of_yojson @@ Yojson.Safe.from_string s)
    , d
  in
  let get' = 
    Caqti_request.collect Caqti_type.(tup3 string ptime ptime) Caqti_type.(tup2 string ptime)
      "SELECT structs, date FROM qoe_structures WHERE input = ?::JSONB AND date > ? AND date <= ? ORDER BY date DESC LIMIT 100"
  in Conn.request db (List get') (i,from,to') >|= (List.map unwrap)

