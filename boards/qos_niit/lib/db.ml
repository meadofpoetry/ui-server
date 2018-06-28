open Containers
open Storage.Database
open Api.Query
open Board_types
open Lwt.Infix
open Common

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
                    ; columns = [ "is_ts",     key "BOOL"
                                ; "stream",    key "INTEGER"
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
                       ~encode:(fun (id,(err : Errors.t)) ->
                         let stream = Common.Stream.id_to_int32 id in
                         Ok (stream, (err.count, (err.err_code, (err.err_ext, (err.priority, (err.multi_pid, (err.pid, (err.packet, (err.param_1,(err.param_2, err.timestamp)))))))))))
                       ~decode:(fun (stream,(count,(err_code,(err_ext,(priority,(multi_pid,(pid,(packet,(param_1,(param_2,timestamp)))))))))) ->
                         let stream = Common.Stream.id_of_int32 stream in
                         Ok (stream,{ timestamp; count; err_code
                                      ; err_ext; priority; multi_pid; pid; packet; param_1; param_2})))

  let max_limit = 500

  let select_errors ?(limit = max_limit) ?from ?til db =
    match from, til with
    | Some f, Some t ->
       let q = List (Caqti_request.collect Types.(tup3 ptime ptime int) error
                       {|SELECT (is_ts,4stream,count,err_code,err_ext,priority,
                                 multi_pid,pid,packet,param_1,param_2,date) FROM qos_niit_errors 
                        WHERE date >= ? AND date <= ? ORDER BY date DESC LIMIT ?|})
       in Conn.request db q (f,t,limit) >|= fun data ->
          Raw { data; has_more = (List.length data >= limit); order = `Desc }
    | Some f, None ->
       let q = List (Caqti_request.collect Types.(tup2 ptime int) error
                       {|SELECT (is_ts,stream,count,err_code,err_ext,priority,
                                 multi_pid,pid,packet,param_1,param_2,date) FROM qos_niit_errors 
                        WHERE date >= ? ORDER BY date ASC LIMIT ?|})
       in Conn.request db q (f,limit) >|= fun data ->
          Raw { data; has_more = (List.length data >= limit); order = `Asc }
    | None, Some t ->
       let q = List (Caqti_request.collect Types.(tup2 ptime int) error
                       {|SELECT (is_ts,stream,count,err_code,err_ext,priority,
                                 multi_pid,pid,packet,param_1,param_2,date) FROM qos_niit_errors 
                        WHERE date <= ? ORDER BY date DESC LIMIT ?|})
       in Conn.request db q (t,limit) >|= fun data ->
          Raw { data; has_more = (List.length data >= limit); order = `Desc }
    | None, None ->
       let q = List (Caqti_request.collect Types.int error
                       {|SELECT (is_ts,stream,count,err_code,err_ext,priority,
                                 multi_pid,pid,packet,param_1,param_2,date) FROM qos_niit_errors 
                        ORDER BY date DESC LIMIT ?|})
       in Conn.request db q limit >|= fun data ->
          Raw { data; has_more = (List.length data >= limit); order = `Desc }

  module TS = struct
    
    let insert_errors db errs =
      let errs = List.map (fun (s,e) -> List.map (Pair.make s) e) errs |> List.concat in
      let insert =
        Caqti_request.exec error
          {|INSERT INTO qos_niit_errors(is_ts,stream,count,err_code,err_ext,priority,
           multi_pid,pid,packet,param_1,param_2,date)
           VALUES (true,?,?,?,?,?,?,?,?,?,?,?)|}
      in Conn.request db (Reduce ((Exec insert),(),(fun () () -> ()))) errs

    let select_errors ?(limit = max_limit) ?from ?til db =
      match from, til with
      | Some f, Some t ->
         let q = List (Caqti_request.collect Types.(tup3 ptime ptime int) error
                         {|SELECT (stream,count,err_code,err_ext,priority,
                          multi_pid,pid,packet,param_1,param_2,date) FROM qos_niit_errors 
                          WHERE is_ts = true AND date >= ? AND date <= ? ORDER BY date DESC LIMIT ?|})
         in Conn.request db q (f,t,limit) >|= fun data ->
            Raw { data; has_more = (List.length data >= limit); order = `Desc }
      | Some f, None ->
         let q = List (Caqti_request.collect Types.(tup2 ptime int) error
                         {|SELECT (stream,count,err_code,err_ext,priority,
                          multi_pid,pid,packet,param_1,param_2,date) FROM qos_niit_errors 
                          WHERE is_ts = true AND date >= ? ORDER BY date ASC LIMIT ?|})
         in Conn.request db q (f,limit) >|= fun data ->
            Raw { data; has_more = (List.length data >= limit); order = `Asc }
      | None, Some t ->
         let q = List (Caqti_request.collect Types.(tup2 ptime int) error
                         {|SELECT (stream,count,err_code,err_ext,priority,
                          multi_pid,pid,packet,param_1,param_2,date) FROM qos_niit_errors 
                          WHERE is_ts = true AND date <= ? ORDER BY date DESC LIMIT ?|})
         in Conn.request db q (t,limit) >|= fun data ->
            Raw { data; has_more = (List.length data >= limit); order = `Desc }
      | None, None ->
         let q = List (Caqti_request.collect Types.int error
                         {|SELECT (stream,count,err_code,err_ext,priority,
                          multi_pid,pid,packet,param_1,param_2,date) FROM qos_niit_errors 
                          WHERE is_ts = true ORDER BY date DESC LIMIT ?|})
         in Conn.request db q limit >|= fun data ->
            Raw { data; has_more = (List.length data >= limit); order = `Desc }
  end

  module T2MI = struct

    let insert_errors db (errs:(Stream.id,Errors.t list) List.Assoc.t) =
      let errs = List.map (fun (s,e) -> List.map (Pair.make s) e) errs |> List.concat in
      let insert =
        Caqti_request.exec error
          {|INSERT INTO qos_niit_errors(is_ts,stream,count,err_code,err_ext,priority,
           multi_pid,pid,packet,param_1,param_2,date)
           VALUES (false,?,?,?,?,?,?,?,?,?,?,?)|}
      in Conn.request db (Reduce ((Exec insert),(),(fun () () -> ()))) errs

    let select_errors ?(limit = max_limit) ?from ?til db =
      match from, til with
      | Some f, Some t ->
         let q = List (Caqti_request.collect Types.(tup3 ptime ptime int) error
                         {|SELECT (stream,count,err_code,err_ext,priority,
                          multi_pid,pid,packet,param_1,param_2,date) FROM qos_niit_errors 
                          WHERE is_ts = false AND date >= ? AND date <= ? ORDER BY date DESC LIMIT ?|})
         in Conn.request db q (f,t,limit) >|= fun data ->
            Raw { data; has_more = (List.length data >= limit); order = `Desc }
      | Some f, None ->
         let q = List (Caqti_request.collect Types.(tup2 ptime int) error
                         {|SELECT (stream,count,err_code,err_ext,priority,
                          multi_pid,pid,packet,param_1,param_2,date) FROM qos_niit_errors 
                          WHERE is_ts = false AND date >= ? ORDER BY date ASC LIMIT ?|})
         in Conn.request db q (f,limit) >|= fun data ->
            Raw { data; has_more = (List.length data >= limit); order = `Asc }
      | None, Some t ->
         let q = List (Caqti_request.collect Types.(tup2 ptime int) error
                         {|SELECT (stream,count,err_code,err_ext,priority,
                          multi_pid,pid,packet,param_1,param_2,date) FROM qos_niit_errors 
                          WHERE is_ts = false AND date <= ? ORDER BY date DESC LIMIT ?|})
         in Conn.request db q (t,limit) >|= fun data ->
            Raw { data; has_more = (List.length data >= limit); order = `Desc }
      | None, None ->
         let q = List (Caqti_request.collect Types.int error
                         {|SELECT (stream,count,err_code,err_ext,priority,
                          multi_pid,pid,packet,param_1,param_2,date) FROM qos_niit_errors 
                          WHERE is_ts = false ORDER BY date DESC LIMIT ?|})
         in Conn.request db q limit >|= fun data ->
            Raw { data; has_more = (List.length data >= limit); order = `Desc }
    
  end
            
        (*
    let select_errors_compress db ?(limit = max_limit) ?from ?til ?(typ = `Any) =
    let query_limit =
      Find (Caqti_request.find Types.int Types.int "SELECT ")
    in*)
     
end
