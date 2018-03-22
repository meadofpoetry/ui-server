open Containers
open Storage.Database
open Lwt.Infix
      
type _ req =
  | Store_structures : Structure.t list -> unit Lwt.t req

let init o =
  let create_video =
    Caqti_request.exec Caqti_type.unit
      {eos|CREATE TABLE IF NOT EXISTS qoe_video_errors(
       black  BOOLEAN,
       luma   BOOLEAN,
       freeze BOOLEAN,
       diff   BOOLEAN,
       blocky BOOLEAN,
       input  TEXT NON NULL,
       value  TEXT,
       date   TIMESTAMP DEFAULT CURRENT_TIMESTAMP
       )|eos}
  in
  Storage.Database.exec o create_video () >>= function
  | Ok v    -> Lwt.return v
  | Error _ -> Lwt.fail_with "init"
             
let store_structures dbs streams =
  let insert =
    Caqti_request.exec Caqti_type.(tup2 string string)
      "INSERT INTO streams(input, value) VALUES (?,?)"
  in
  let s = Structure_conv.dump_streams streams in
  let store_structures' (module Db : Caqti_lwt.CONNECTION) =
    List.fold_left
      (fun thread (i,v) ->
        thread >>= function
        | Ok ()  -> Db.exec insert (i, v)
        | _ as e -> Lwt.return e)
      (Lwt.return_ok ()) s
    >>= function
    | Ok v    -> Lwt.return v
    | Error _ -> Lwt.fail_with "store_structures"
  in Storage.Database.with_connection dbs store_structures'

let request (type a) dbs (r : a req) : a =
  match r with
  | Store_structures s -> store_structures dbs s
