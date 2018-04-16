open Containers
open Storage.Database
open Lwt.Infix
   
type _ req =
  | Store_structures : Structure.t list -> unit req

let name = "pipeline"
  
let init (module Db : Caqti_lwt.CONNECTION) =
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
  Db.exec create_video () >>= function
  | Ok v    -> Lwt.return v
  | Error _ -> Lwt.fail_with "pipeline: init"

let store_structures (module Db : Caqti_lwt.CONNECTION) streams =
  let insert =
    Caqti_request.exec Caqti_type.(tup2 string string)
                       "INSERT INTO streams(input, value) VALUES (?,?)"
  in
  let s = Structure_conv.dump_streams streams in
  let store_structures' (module Db : Caqti_lwt.CONNECTION) =
    List.fold_left
      (fun thread (i,v) ->
        thread >>= function
        | Ok ()  -> Db.exec insert (Common.Uri.to_string i, v)
        | _ as e -> Lwt.return e)
      (Lwt.return_ok ()) s
    >>= function
    | Ok v    -> Lwt.return v
    | Error _ -> Lwt.fail_with "pipeline: store_structures"
  in store_structures' (module Db)

let request (type a) dbs (r : a req) : a Lwt.t =
  match r with
  | Store_structures s -> store_structures dbs s

let cleanup (module Db : Caqti_lwt.CONNECTION) =
  let cleanup' =
    Caqti_request.exec Caqti_type.unit
      "DELETE FROM qoe_video_errors WHERE date <= date('now','-2 day')"
  in
  Db.exec cleanup' () >>= function
  | Ok ()   -> Lwt.return ()
  | Error _ -> Lwt.fail_with "pipeline: cleanup"

let delete (module Db : Caqti_lwt.CONNECTION) =
  let delete' =
    Caqti_request.exec Caqti_type.unit
      "DELETE FROM qoe_video_errors"
  in
  Db.exec delete' () >>= function
  | Ok ()   -> Lwt.return ()
  | Error _ -> Lwt.fail_with "pipeline: delete"

let worker = None
