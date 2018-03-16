open Containers
open Storage.Database
open Board_types
open Lwt.Infix
   
type _ req =
  | Store_measures : Board_types.measure_response -> unit Lwt.t req

let measure = Caqti_type.custom
                Caqti_type.(let (&) = tup2 in
                            int & bool & (option float) & (option float) & (option float) & (option int32) & (option int32) & int32)
                ~encode:(fun (id, m) ->
                  Ok(id,(m.lock,(m.power,(m.mer,(m.ber,(m.freq,(m.bitrate,(Int32.of_float m.timestamp)))))))))
                ~decode:(fun (id,(lock,(power,(mer,(ber,(freq,(bitrate,(timestamp)))))))) ->
                  Ok(id, { lock; power; mer; ber; freq; bitrate; timestamp = (Int32.to_float timestamp)}))
  
let init o =
  let create =
    Caqti_request.exec Caqti_type.unit
      {eos|
       CREATE TABLE IF NOT EXISTS dvb_meas(
       tun     INTEGER,
       lock    BOOL,
       power   REAL,
       mer     REAL,
       ber     REAL,
       freq    INTEGER,
       bitrate INTEGER,
       date    TIMESTAMP DEFAULT CURRENT_TIMESTAMP
       )
       |eos}
  in
  let init' (module Db : Caqti_lwt.CONNECTION) =
    Db.exec create () >>= function
    | Ok v    -> Lwt.return v
    | Error _ -> Lwt.fail_with "init"
  in Storage.Database.with_connection o init'

let store_measures o (id,m) =
  let insert =
    Caqti_request.exec measure
      {|INSERT INTO dvb_meas(tun,lock,power,mer,ber,freq,bitrate,date)
       VALUES (?,?,?,?,?,?,?,?)|}
  in
  let store_measures' (module Db : Caqti_lwt.CONNECTION) =
    Db.exec insert (id,m) >>= function
    | Ok v    -> Lwt.return v
    | Error _ -> Lwt.fail_with "store_measure"
  in Storage.Database.with_connection o store_measures'

let request (type a) o (req : a req) : a =
  match req with
  | Store_measures m -> store_measures o m
