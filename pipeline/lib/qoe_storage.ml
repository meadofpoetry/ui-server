open Containers
open Storage.Database
open Qoe_errors
open Lwt.Infix

   
let fail_if = function Ok v -> Lwt.return v | Error e -> Lwt.fail_with (Caqti_error.show e)

let data_t =
  Caqti_type.custom
    Caqti_type.(let (&) = tup2 in
                int & int & int & int
                & int & int
                & float & float & float
                & bool & bool & ptime)
    ~encode:(fun (stream,channel,pid,error,data) ->
      let (&) a b = (a, b) in
      Ok(stream & channel & pid & error & data.counter & data.size
         & data.params.min & data.params.max & data.params.avg 
         & data.peak_flag & data.cont_flag & (Option.get_exn @@ Common.Time.of_span data.timestamp)))
    ~decode:(fun (stream,(channel,(pid,(error,(counter,(size,(min,(max,(avg,(peak_flag,(cont_flag,timestamp))))))))))) ->
      Ok(stream,channel,pid,error, { counter; size; params = { min; max; avg }; peak_flag;
                                     cont_flag; timestamp = Common.Time.to_span timestamp }))
                                

module Model = struct
  let name = "qoe"
  let init =
    Exec (Caqti_request.exec Caqti_type.unit
            {eos|CREATE TABLE IF NOT EXISTS qoe_errors(
             stream  INTEGER,  channel INTEGER,  pid     INTEGER,
             error   INTEGER,  counter INTEGER,  size    INTEGER,
             min     REAL,     max     REAL,     avg     REAL,
             peak_flag BOOLEAN, cont_flag BOOLEAN, date   TIMESTAMP DEFAULT CURRENT_TIMESTAMP
             )|eos})

  let tables = ["qoe_errors", init, None]
end

module Conn = Storage.Database.Make(Model)
             
let insert_data db data =
  let insert =
    Caqti_request.exec data_t
      {eos|INSERT INTO qoe_errors(stream,channel,pid,error,counter,size,min,max,avg,peak_flag,cont_flag,date)
       VALUES (?,?,?,?,?,?,?,?,?,?,?,?)|eos}
  in Conn.request db (Reduce ((Exec insert),(),(fun x _ -> x))) data

let insert_audio db data = insert_data db (audio_data_to_list data)

let insert_video db data = insert_data db (video_data_to_list data)
