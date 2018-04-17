open Containers
open Storage.Database
open Common

open Lwt.Infix
   
type _ req =
  | Store : Structure.t list -> unit req
  | Get_input : Common.Topology.topo_input -> (Structure.t list * Time.t) req
  | Get_input_between : (Common.Topology.topo_input * Time.t * Time.t) -> (Structure.t list * Time.t) list req

let name = "pipeline"

let table = "qoe_structures"

let fail_if = function Ok v -> Lwt.return v | Error e -> Lwt.fail_with (Caqti_error.show e)
  
let init (module Db : Caqti_lwt.CONNECTION) =
  let create_video =
    Caqti_request.exec Caqti_type.unit
      {eos|CREATE TABLE IF NOT EXISTS qoe_structures(
       input   JSONB,
       structs JSONB,
       date    TIMESTAMP DEFAULT CURRENT_TIMESTAMP
       )|eos}
  in
  Db.exec create_video () >>= function
  | Ok v    -> Lwt.return v
  | Error e -> Lwt.fail_with (error "pipeline: init %s" e)

let store_structures (module Db : Caqti_lwt.CONNECTION) streams =
  let insert =
    Caqti_request.exec Caqti_type.(tup2 string string)
                       "INSERT INTO qoe_structures(input, structs) VALUES (?,?)"
  in
  let entries = Structure_conv.dump_structures streams in
  let store' (module Db : Caqti_lwt.CONNECTION) =
    Db.start () >>= fail_if >>= fun () ->
    List.fold_left
      (fun thread (i,v) ->
        thread >>= function
        | Ok ()  -> Db.exec insert (i, v)
        | _ as e -> Lwt.return e)
      (Lwt.return_ok ()) entries
    >>= fail_if >>= Db.commit >>= function
    | Ok v    -> Lwt.return v
    | Error e -> Lwt.fail_with (error "pipeline: store_structures %s" e)
  in store' (module Db)

let get_input (module Db : Caqti_lwt.CONNECTION) i =
  let i = Yojson.Safe.to_string @@ Common.Topology.topo_input_to_yojson i in
  let unwrap (s,d) =
    (Result.get_exn @@ Structure.Streams.of_yojson @@ Yojson.Safe.from_string s)
    , d
  in
  let get' =
    Caqti_request.find Caqti_type.string Caqti_type.(tup2 string ptime)
      "SELECT structs, date FROM qoe_structures WHERE input = ?::JSONB ORDER BY date DESC LIMIT 1"
  in
  Db.find get' i >>= function
  | Ok v    -> Lwt.return (unwrap v)
  | Error e -> Lwt.fail_with (error "structure_storage.get_input: %s" e)

let get_input_between (module Db : Caqti_lwt.CONNECTION) i from to' =
  let i = Yojson.Safe.to_string @@ Common.Topology.topo_input_to_yojson i in
  let unwrap (s,d) =
    (Result.get_exn @@ Structure.Streams.of_yojson @@ Yojson.Safe.from_string s)
    , d
  in
  let get' = 
    Caqti_request.collect Caqti_type.(tup3 string ptime ptime) Caqti_type.(tup2 string ptime)
      "SELECT structs, date FROM qoe_structures WHERE input = ?::JSONB AND date > ? AND date <= ? ORDER BY date DESC LIMIT 100"
  in
  Db.collect_list get' (i,from,to') >>= function
  | Ok v    -> Lwt.return (List.map unwrap v)
  | Error e -> Lwt.fail_with (error "structure_storage.get_input: %s" e)

let request (type a) dbs (r : a req) : a Lwt.t =
  match r with
  | Store s -> store_structures dbs s
  | Get_input i -> get_input dbs i
  | Get_input_between (i,from,to') -> get_input_between dbs i from to'

let worker = None
