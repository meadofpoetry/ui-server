open Containers
open Storage.Database
open Board_types
open Lwt.Infix

let measure = Caqti_type.custom
                Caqti_type.(let (&) = tup2 in
                            int & bool & (option float) & (option float) & (option float)
                            & (option int32) & (option int32) & ptime)
                ~encode:(fun (id, m) ->
                  Ok(id,(m.lock,(m.power,(m.mer,(m.ber,(m.freq,(m.bitrate,m.timestamp))))))))
                ~decode:(fun (id,(lock,(power,(mer,(ber,(freq,(bitrate,(timestamp)))))))) ->
                  Ok(id, { lock; power; mer; ber; freq; bitrate; timestamp }))
   
module Model = struct
  let name = "dvb_niit"
  let table = "dvb_meas"
  let init =
    Exec (Caqti_request.exec Caqti_type.unit
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
             |eos})
  let worker = None
end

module Conn = Storage.Database.Make(Model)

type t = Conn.t
    
let insert_measures db (id,m) =
  let insert =
    Caqti_request.exec measure
      {|INSERT INTO dvb_meas(tun,lock,power,mer,ber,freq,bitrate,date)
       VALUES (?,?,?,?,?,?,?,'epoch'::TIMESTAMP + ? * '1 second'::INTERVAL)|}
  in Conn.request db (Exec insert) (id,m)
