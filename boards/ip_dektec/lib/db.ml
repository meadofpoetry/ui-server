open Containers
open Storage.Database
open Board_types
open Lwt.Infix

let name = "ip_dektec"
   
type _ req =
  | Store_status : Board_types.board_status -> unit req

let init (module Db : Caqti_lwt.CONNECTION) =
  let create =
    Caqti_request.exec Caqti_type.unit
      {eos|CREATE TABLE IF NOT EXISTS ip_status(
       bitrate INTEGER,
       fec_delay INTEGER,
       pcr_present BOOL,
       date   TIMESTAMP DEFAULT CURRENT_TIMESTAMP
       )|eos}
  in
  Db.exec create () >>= function
  | Ok v    -> Lwt.return v
  | Error _ -> Lwt.fail_with "ip_dektec: init"

let store_status (module Db : Caqti_lwt.CONNECTION) (s : board_status) =
  let insert =
    Caqti_request.exec Caqti_type.(tup3 int int bool)
      {|INSERT INTO ip_status(bitrate,fec_delay,pcr_present)
       VALUES (?,?,?)|}
  in
  Db.exec insert (s.bitrate, s.fec_delay, s.pcr_present) >>= function
  | Ok v    -> Lwt.return v
  | Error _ -> Lwt.fail_with "ip_dektec: store_status"

let request (type a) o (req : a req) : a Lwt.t =
  match req with
  | Store_status s -> store_status o s

let cleanup (module Db : Caqti_lwt.CONNECTION) =
  let cleanup' =
    Caqti_request.exec Caqti_type.unit
      "DELETE FROM ip_status WHERE date <= date('now','-2 day')"
  in
  Db.exec cleanup' () >>= function
  | Ok ()   -> Lwt.return ()
  | Error _ -> Lwt.fail_with "ip_dektec: cleanup"

let delete (module Db : Caqti_lwt.CONNECTION) =
  let delete' =
    Caqti_request.exec Caqti_type.unit
      "DELETE FROM ip_status"
  in
  Db.exec delete' () >>= function
  | Ok ()   -> Lwt.return ()
  | Error _ -> Lwt.fail_with "ip_dektec: delete"

let worker = None
