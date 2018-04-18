open Containers
open Storage.Database
open Common

open Lwt.Infix
   
type _ req =
  | Store : Structure.t list -> unit req
  | Get_input : Common.Topology.topo_input -> (Structure.t list * Time.t) option req
  | Get_input_between : (Common.Topology.topo_input * Time.t * Time.t) -> (Structure.t list * Time.t) list req

let name = "pipeline"

let table = "qoe_structures"
  
let init db : unit Lwt.t =
  let insert = Caqti_request.exec Caqti_type.unit
                 {eos|CREATE TABLE IF NOT EXISTS qoe_structures(
                  input   JSONB,
                  structs JSONB,
                  date    TIMESTAMP DEFAULT CURRENT_TIMESTAMP
                  )|eos}
  in db.exec (Exec insert) ()

let store_structures db streams : unit Lwt.t =
  let entries = Structure_conv.dump_structures streams in
  let insert  = Caqti_request.exec Caqti_type.(tup2 string string)
                  "INSERT INTO qoe_structures(input, structs) VALUES (?,?)"
  in db.exec (Trans ((Exec insert),(),(fun x _ -> x))) entries

let get_input db i =
  let i = Yojson.Safe.to_string @@ Common.Topology.topo_input_to_yojson i in
  let unwrap (s,d) =
    (Result.get_exn @@ Structure.Streams.of_yojson @@ Yojson.Safe.from_string s)
    , d
  in
  let get' =
    Caqti_request.find Caqti_type.string Caqti_type.(tup2 string ptime)
      "SELECT structs, date FROM qoe_structures WHERE input = ?::JSONB ORDER BY date DESC LIMIT 1"
  in db.exec (Find get') i >|= (Option.map unwrap)

let get_input_between db i from to' =
  let i = Yojson.Safe.to_string @@ Common.Topology.topo_input_to_yojson i in
  let unwrap (s,d) =
    (Result.get_exn @@ Structure.Streams.of_yojson @@ Yojson.Safe.from_string s)
    , d
  in
  let get' = 
    Caqti_request.collect Caqti_type.(tup3 string ptime ptime) Caqti_type.(tup2 string ptime)
      "SELECT structs, date FROM qoe_structures WHERE input = ?::JSONB AND date > ? AND date <= ? ORDER BY date DESC LIMIT 100"
  in db.exec (List get') (i,from,to') >|= (List.map unwrap)

let request (type a) db (r :  a req) : a Lwt.t =
  match r with
  | Store s -> store_structures db s
  | Get_input i -> get_input db i
  | Get_input_between (i,from,to') -> get_input_between db i from to'

let worker = None
