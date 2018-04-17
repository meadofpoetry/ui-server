open Containers
open Storage.Database
open Qoe_errors
open Lwt.Infix
   
type _ req =
  | Store_video : Video_data.t -> unit req
  | Store_audio : Audio_data.t -> unit req

let name = "qoe"

let table = "qoe_errors"

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
         & data.peak_flag & data.cont_flag & data.timestamp))
    ~decode:(fun (stream,(channel,(pid,(error,(counter,(size,(min,(max,(avg,(peak_flag,(cont_flag,timestamp))))))))))) ->
      Ok(stream,channel,pid,error, { counter; size; params = { min; max; avg }; peak_flag; cont_flag; timestamp }))
                                                                               
let init (module Db : Caqti_lwt.CONNECTION) =
  let create =
    Caqti_request.exec Caqti_type.unit
      {eos|CREATE TABLE IF NOT EXISTS qoe_errors(
       stream  INTEGER,  channel INTEGER,  pid     INTEGER,
       error   INTEGER,  counter INTEGER,  size    INTEGER,
       min     REAL,     max     REAL,     avg     REAL,
       peak_flag BOOLEAN, cont_flag BOOLEAN, date   TIMESTAMP DEFAULT CURRENT_TIMESTAMP
       )|eos}
  in
  Db.exec create () >>= function
  | Ok v    -> Lwt.return v
  | Error e -> Lwt.fail_with (error "qoe_storage: init %s" e)

let store (module Db : Caqti_lwt.CONNECTION) data =
  let insert =
    Caqti_request.exec data_t
      {eos|INSERT INTO qoe_errors(stream,channel,pid,error,counter,size,min,max,avg,peak_flag,cont_flag,date)
       VALUES (?,?,?,?,?,?,?,?,?,?,?,?)|eos}
  in
  let store' (module Db : Caqti_lwt.CONNECTION) =
    Db.start () >>= fail_if >>= fun () ->
    List.fold_left
      (fun thread v ->
        thread >>= function
        | Ok ()  -> Db.exec insert v
        | _ as e -> Lwt.return e)
      (Lwt.return_ok ()) data
    >>= fail_if >>= Db.commit >>= function
    | Ok v    -> Lwt.return v
    | Error e -> Lwt.fail_with (error "qoe_storage: store_data %s" e)
  in store' (module Db)

let request (type a) dbs (r : a req) : a Lwt.t =
  match r with
  | Store_video s -> store dbs (video_data_to_list s)
  | Store_audio s -> store dbs (audio_data_to_list s)

let worker = None
