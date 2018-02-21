open Containers
open Storage.Database
open Board_types
open Lwt.Infix

type _ req =
  | Store_status : Board_types.board_status -> unit Lwt.t req

let init o =
  let create =
    Caqti_request.exec Caqti_type.unit
      {eos|CREATE TABLE IF NOT EXISTS ip_status(
       bitrate INTEGER,
       fec_delay INTEGER,
       pcr_present BOOL,
       date   TIMESTAMP DEFAULT CURRENT_TIMESTAMP
       )|eos}
  in
  let (module Db) = Storage.Database.connection o in
  Db.exec create () >>= function
  | Ok v    -> Lwt.return v
  | Error _ -> Lwt.fail_with "init"

let store_status o (s : board_status) =
  let insert =
    Caqti_request.exec Caqti_type.(tup3 int int bool)
      {|INSERT INTO ip_status(bitrate,fec_delay,pcr_present)
       VALUES (?,?,?)|}
  in
  let (module Db) = Storage.Database.connection o in
  Db.exec insert (s.bitrate, s.fec_delay, s.pcr_present) >>= function
  | Ok v    -> Lwt.return v
  | Error _ -> Lwt.fail_with "init"

let request (type a) o (req : a req) : a =
  match req with
  | Store_status s -> store_status o s
