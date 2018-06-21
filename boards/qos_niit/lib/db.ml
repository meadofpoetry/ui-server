open Containers
open Storage.Database
open Board_types
open Lwt.Infix

module Model = struct
  open Key_t
  let name = "qos_niit"

  let keys_state = { time_key = Some "date_end"
                   ; columns  = [ "status", key "INTEGER"
                                ; "date_start", key "TIMESTAMP"
                                ; "date_end",   key "TIMESTAMP"
                                ]
                   }

  let keys_errors = { time_key = Some "date"
                    ; columns = [ "stream",    key "INTEGER"
                                ; "count",     key "INTEGER"
                                ; "err_code",  key "INTEGER"
                                ; "err_ext",   key "INTEGER"
                                ; "priority",  key "INTEGER"
                                ; "multi_pid", key "BOOL"
                                ; "pid",       key "INTEGER"
                                ; "packet",    key "INTEGER"
                                ; "param_1",   key "INTEGER"
                                ; "param_2",   key "INTEGER"
                                ; "date",      key "TIMESTAMP"
                                ]
                    }
                 
  let tables = [ "qos_niit_state", keys_state, None
               ; "qos_niit_errors", keys_errors, None
               ]
           
end

module Conn = Storage.Database.Make(Model)

type t = Conn.t

module Status = struct
  
  
end
       
module Errors = struct
  open Board_types.Errors

  let error = Types.(custom (int32 & int & int & int & int & bool & int & int32 & int32 & int32 & ptime)
                       ~encode:(fun (err : Errors.t) ->
                         let stream = Common.Stream.id_to_int32 err.stream in
                         Ok (stream, (err.count, (err.err_code, (err.err_ext, (err.priority, (err.multi_pid, (err.pid, (err.packet, (err.param_1,(err.param_2, err.timestamp)))))))))))
                       ~decode:(fun (stream,(count,(err_code,(err_ext,(priority,(multi_pid,(pid,(packet,(param_1,(param_2,timestamp)))))))))) ->
                         let stream = Common.Stream.id_of_int32 stream in
                         Ok { stream; timestamp; count; err_code
                              ; err_ext; priority; multi_pid; pid; packet; param_1; param_2}))
            
  let insert_errors db err_list =
    let insert =
      Caqti_request.exec error
        {|INSERT INTO qos_niit_errors(stream,count,err_code,err_ext,priority,multi_pid,pid,packet,param_1,param_2,date)
         VALUES (?,?,?,?,?,?,?,?,?,?,?)|}
    in Conn.request db (Reduce ((Exec insert),(),(fun () () -> ()))) err_list

  let select_errors db = ()
     
end
