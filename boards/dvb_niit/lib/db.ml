open Storage.Database
open Board_types
open Lwt.Infix
   
type _ req =
  | Store_measures : Board_types.measure -> unit Lwt.t req

let init o =
  Storage.Database.execute o [%sqlinit {|CREATE TABLE IF NOT EXISTS dvb_meas(
                                        tun     INTEGER,
                                        lock    BOOL,
                                        power   REAL,
                                        mer     REAL,
                                        ber     REAL,
                                        freq    INTEGER,
                                        bitrate INTEGER,
                                        date   TIMESTAMP DEFAULT CURRENT_TIMESTAMP
                                        );|} ]
  >>= fun _ -> Lwt.return_unit

let store_measures o (i, (m : rsp_measure)) =
  Storage.Database.insert o [%sqlc {|INSERT INTO dvb_meas(tun,lock,power,mer,ber,freq,bitrate)
                                    VALUES (%d,%b,%f?,%f?,%f?,%l?,%l?)|}]
    i m.lock m.power m.mer m.ber m.freq m.bitrate
  >>= fun _ -> Lwt.return_unit
             
let request (type a) o (req : a req) : a =
  match req with
  | Store_measures m -> store_measures o m
