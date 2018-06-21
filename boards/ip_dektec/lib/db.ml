(*

open Containers
open Storage.Database
open Board_types
open Lwt.Infix

module Model : Storage.Database.MODEL = struct
  let name = "ip_dektec"
  let init =
    Exec (Caqti_request.exec Caqti_type.unit
            {eos|CREATE TABLE IF NOT EXISTS ip_status(
             bitrate INTEGER,
             fec_delay INTEGER,
             pcr_present BOOL,
             date   TIMESTAMP DEFAULT CURRENT_TIMESTAMP
             )|eos})

  let tables = ["ip_status", init, None]
end

module Conn = Storage.Database.Make(Model)

type t = Conn.t
             
let insert_status db (s : board_status) =
  let insert =
    Caqti_request.exec Caqti_type.(tup3 int int bool)
      {|INSERT INTO ip_status(bitrate,fec_delay,pcr_present)
       VALUES (?,?,?)|}
  in Conn.request db (Exec insert) (s.bitrate, s.fec_delay, s.pcr_present)
 *)
