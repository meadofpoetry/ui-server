open Containers
open Storage.Database
open Board_types
open Lwt.Infix

let name = "ip_dektec"

let table = "ip_status"
   
type _ req =
  | Store_status : Board_types.board_status -> unit req

let init db =
  let create =
    Caqti_request.exec Caqti_type.unit
      {eos|CREATE TABLE IF NOT EXISTS ip_status(
       bitrate INTEGER,
       fec_delay INTEGER,
       pcr_present BOOL,
       date   TIMESTAMP DEFAULT CURRENT_TIMESTAMP
       )|eos}
  in db.exec (Exec create) ()
   
let store_status db (s : board_status) =
  let insert =
    Caqti_request.exec Caqti_type.(tup3 int int bool)
      {|INSERT INTO ip_status(bitrate,fec_delay,pcr_present)
       VALUES (?,?,?)|}
  in db.exec (Exec insert) (s.bitrate, s.fec_delay, s.pcr_present)

let request (type a) o (req : a req) : a Lwt.t =
  match req with
  | Store_status s -> store_status o s

let worker = None
