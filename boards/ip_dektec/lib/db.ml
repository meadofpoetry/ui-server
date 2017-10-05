open Storage.Database
open Board_types
open Lwt.Infix

type _ req =
  | Store_status : Board_types.board_status -> unit Lwt.t req

let init o =
  Storage.Database.execute o [%sqlinit {|CREATE TABLE IF NOT EXISTS ip_status(
                                        bitrate INTEGER,
                                        fec_delay INTEGER,
                                        pcr_present BOOL,
                                        date   TIMESTAMP DEFAULT CURRENT_TIMESTAMP
                                        );|} ]
  >>= fun _ -> Lwt.return_unit

let store_status o (s : board_status) =
  Storage.Database.insert o [%sqlc {|INSERT INTO ip_status(bitrate,fec_delay,pcr_present)
                                    VALUES (%d,%d,%b)|} ]
    s.bitrate s.fec_delay s.pcr_present
  >>= fun _ -> Lwt.return_unit

let request (type a) o (req : a req) : a =
  match req with
  | Store_status s -> store_status o s
