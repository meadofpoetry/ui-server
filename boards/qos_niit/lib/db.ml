open Containers
open Storage.Database
open Api.Api_types
open Board_types
open Lwt.Infix
open Common

module R = Caqti_request

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

  let error = Types.(custom List.(int32 & int & int & int & int & bool & int & int32 & int32 & int32 & ptime)
                       ~encode:(fun (id,(err : Errors.t)) ->
                         let stream = Common.Stream.id_to_int32 id in
                         Ok (stream, (err.count, (err.err_code, (err.err_ext, (err.priority, (err.multi_pid, (err.pid, (err.packet, (err.param_1,(err.param_2, err.timestamp)))))))))))
                       ~decode:(fun (stream,(count,(err_code,(err_ext,(priority,(multi_pid,(pid,(packet,(param_1,(param_2,timestamp)))))))))) ->
                         let stream = Common.Stream.id_of_int32 stream in
                         Ok (stream,{ timestamp; count; err_code
                                      ; err_ext; priority; multi_pid; pid; packet; param_1; param_2})))

  let insert_errors db ~is_ts errs =
    let errs = List.map (fun (s,e) -> List.map (Pair.make s) e) errs |> List.concat in
    let insert =
      R.exec Types.(tup2 bool error)
        {|INSERT INTO qos_niit_errors(is_ts,stream,count,err_code,err_ext,priority,
         multi_pid,pid,packet,param_1,param_2,date)
         VALUES (?,?,?,?,?,?,?,?,?,?,?,?)|}
    in Conn.request db Request.(with_trans (List.fold_left (fun acc x -> acc >>= fun () -> exec insert (is_ts,x))
                                              (return ()) errs))

  let is_in field to_string = function
    | [] -> ""
    | lst -> Printf.sprintf " %s IS IN (%s) AND " field (String.concat "," @@ List.map to_string lst)
     
  let select_has_any db ?(streams = []) ?(priority = []) ?(pids = []) ?(errors = []) ~is_ts ~from ~till () =
    let streams  = is_in "stream" Int32.to_string streams in
    let priority = is_in "priority" string_of_int priority in
    let pids     = is_in "pid" string_of_int pids in
    let errors   = is_in "err_code"  string_of_int errors in
    let select = R.find Types.(tup3 bool ptime ptime) Types.bool
                   (Printf.sprintf {|SELECT TRUE FROM qos_niit_errors
                                    WHERE %s %s %s %s is_ts = ? AND date >= ? AND date <= ? LIMIT 1|}
                      streams priority pids errors)
    in Conn.request db Request.(find select (is_ts,from,till) >>= function None -> return false | Some x -> return x)

  let select_percent db ?(streams = []) ?(priority = []) ?(pids = []) ?(errors = []) ~is_ts ~from ~till () =
    let streams  = is_in "stream" Int32.to_string streams in
    let priority = is_in "priority" string_of_int priority in
    let pids     = is_in "pid" string_of_int pids in
    let errors   = is_in "err_code"  string_of_int errors in
    let span = Time.(Period.to_float_s @@ diff till from) in
    let select = R.find Types.(tup3 bool ptime ptime) Types.int
                   (Printf.sprintf {|SELECT count(DISTINCT date_trunc('second',date)) FROM qos_niit_errors 
                                    WHERE %s %s %s %s is_ts = ? AND date >= ? AND date <= ?|}
                      streams priority pids errors)
    in Conn.request db Request.(find select (is_ts,from,till) >>= function
                                | None -> return 0.0
                                | Some s -> return (100. *. (float_of_int s) /. span))

  let max_limit = 500
                
  let select_errors db ?(streams = []) ?(priority = []) ?(pids = []) ?(errors = []) ?(limit = max_limit)
        ~is_ts ~from ~till () =
    let streams  = is_in "stream" Int32.to_string streams in
    let priority = is_in "priority" string_of_int priority in
    let pids     = is_in "pid" string_of_int pids in
    let errors   = is_in "err_code"  string_of_int errors in
    let select = R.collect Types.(tup4 bool ptime ptime int) error
                   (Printf.sprintf {|SELECT stream,count,err_code,err_ext,priority,
                                    multi_pid,pid,packet,param_1,param_2,date FROM qos_niit_errors 
                                    WHERE %s %s %s %s is_ts = ? AND date >= ? AND date <= ?
                                    ORDER BY date DESC LIMIT ?|}
                      streams priority pids errors)
    in Conn.request db Request.(list select (is_ts,from,till,limit) >>= fun data ->
                                return (Raw { data; has_more = (List.length data >= limit); order = `Desc }))

  type err = (Common.Stream.id * Errors.t) list [@@deriving yojson]
     
  type per = (float * Time.t * Time.t) list [@@deriving yojson]
     
  let select_errors_compressed db ?(streams = []) ?(priority = []) ?(pids = []) ?(errors = []) ~is_ts ~from ~till () =
    let streams  = is_in "stream" Int32.to_string streams in
    let priority = is_in "priority" string_of_int priority in
    let pids     = is_in "pid" string_of_int pids in
    let errors   = is_in "err_code"  string_of_int errors in
    let intvals = Time.split ~from ~till in
    let select = R.find Types.(tup3 bool ptime ptime ) Types.int
                   (Printf.sprintf {|SELECT count(DISTINCT date_trunc('second',date)) FROM qos_niit_errors
                                    WHERE %s %s %s %s is_ts = ? AND date >= ? AND date <= ?|}
                      streams priority pids errors)
    in Conn.request db Request.(with_trans (List.fold_left (fun acc (f,t) ->
                                                let span = Time.(Period.to_float_s @@ diff till from) in
                                                acc >>= fun l ->
                                                find select (is_ts, f, t) >>= function
                                                | None -> return l
                                                | Some s -> let perc = (100. *. (float_of_int s) /. span) in
                                                            return ((perc,f,t)::l))
                                              (return []) intvals)
                                >>= fun l -> return (Compressed { data = List.rev l }))

end
