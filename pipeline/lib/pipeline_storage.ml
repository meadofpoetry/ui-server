open Containers
open Storage.Database
open Lwt.Infix
      
type _ req =
  | Store_structures : Structure.t list -> unit Lwt.t req

let init o =
  let create =
    Caqti_request.exec Caqti_type.unit
      {eos|CREATE TABLE IF NOT EXISTS streams(
       input  TEXT NON NULL,
       value  TEXT,
       date   TIMESTAMP DEFAULT CURRENT_TIMESTAMP
       )|eos}
  in
  let (module Db) = Storage.Database.connection o in
  Db.exec create () >>= function
  | Ok v    -> Lwt.return v
  | Error _ -> Lwt.fail_with "init"
             
let store_structures dbs streams =
  let insert =
    Caqti_request.exec Caqti_type.(tup2 string string)
      "INSERT INTO streams(input, value) VALUES (?,?)"
  in
  let s = Structure_conv.dump_streams streams in
  let (module Db) = Storage.Database.connection dbs in
  List.fold_left
    (fun thread (i,v) ->
      thread >>= function
      | Ok ()  -> Db.exec insert (i, v)
      | _ as e -> Lwt.return e)
    (Lwt.return_ok ()) s
  >>= function
  | Ok v    -> Lwt.return v
  | Error _ -> Lwt.fail_with "store_structures"

let request (type a) dbs (r : a req) : a =
  match r with
  | Store_structures s -> store_structures dbs s
