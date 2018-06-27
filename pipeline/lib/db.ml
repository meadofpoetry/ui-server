open Containers
open Storage.Database
open Common
open Qoe_errors
   
open Lwt.Infix

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
  open Key_t
     
  let name = "qoe(pipeline)"

  let struct_keys  = { time_key = Some "date"
                     ; columns  = [ "input",   key "JSONB"
                                  ; "structs", key "JSONB"
                                  ; "date",    key "TIMESTAMP" ~default:"CURRENT_TIMESTAMP"
                                  ]
                     }

  let err_keys = { time_key = Some "date"
                 ; columns = [ "stream", key "INTEGER"
                             ; "channel", key "INTEGER"
                             ; "pid",     key "INTEGER"
                             ; "error",   key "INTEGER"
                             ; "counter", key "INTEGER"
                             ; "size",    key "INTEGER"
                             ; "min",     key "REAL"
                             ; "max",     key "REAL"
                             ; "avg",     key "REAL"
                             ; "peak_flag", key "BOOLEAN"
                             ; "cont_flag", key "BOOLEAN"
                             ; "date",      key "TIMESTAMP" ~default:"CURRENT_TIMESTAMP"
                             ]
                 }
        
  let tables = [ "qoe_structures", struct_keys, None
               ; "qoe_errors", err_keys, None
               ]
end

module Conn = Storage.Database.Make(Model)

module Structure = struct
  let insert_structures db streams : unit Lwt.t =
    let entries = Structure_conv.dump_structures streams in
    let insert  = Caqti_request.exec Caqti_type.(tup2 string string)
                    "INSERT INTO qoe_structures(input, structs) VALUES (?,?)"
    in Conn.request db (Reduce ((Exec insert),(),(fun x _ -> x))) entries

  let select_input db i =
    let i = Yojson.Safe.to_string @@ Common.Topology.topo_input_to_yojson i in
    let unwrap (s,d) =
      (Result.get_exn @@ Structure.Streams.of_yojson @@ Yojson.Safe.from_string s)
      , d
    in
    let get' =
      Caqti_request.find Caqti_type.string Caqti_type.(tup2 string ptime)
        "SELECT structs, date FROM qoe_structures WHERE input = ?::JSONB ORDER BY date DESC LIMIT 1"
    in Conn.request db (Find get') i >|= (Option.map unwrap)

  let select_input_between db i from to' =
    let i = Yojson.Safe.to_string @@ Common.Topology.topo_input_to_yojson i in
    let unwrap (s,d) =
      (Result.get_exn @@ Structure.Streams.of_yojson @@ Yojson.Safe.from_string s)
      , d
    in
    let get' = 
      Caqti_request.collect Caqti_type.(tup3 string ptime ptime) Caqti_type.(tup2 string ptime)
        "SELECT structs, date FROM qoe_structures WHERE input = ?::JSONB AND date > ? AND date <= ? ORDER BY date DESC LIMIT 100"
    in Conn.request db (List get') (i,from,to') >|= (List.map unwrap)

end

module Errors = struct
  let insert_data db data =
  let insert =
    Caqti_request.exec data_t
      {eos|INSERT INTO qoe_errors(stream,channel,pid,error,counter,size,min,max,avg,peak_flag,cont_flag,date)
       VALUES (?,?,?,?,?,?,?,?,?,?,?,?)|eos}
  in Conn.request db (Reduce ((Exec insert),(),(fun x _ -> x))) data

  let insert_audio db data = insert_data db (audio_data_to_list data)

  let insert_video db data = insert_data db (video_data_to_list data)

end
