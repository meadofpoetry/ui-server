open Containers
open Storage.Database
open Api.Api_types
open Board_types
open Lwt.Infix
open Common
open Printf

module R = Caqti_request

module Model = struct
  open Key_t
     
  type init = int
  type names = { state : string
               ; streams : string
               ; struct_ts : string
               ; struct_t2 : string
               ; bitrate : string
               ; errors : string
               }
            
  let name = "qos_niit"

  let keys_state = { time_key = Some "date_end"
                   ; columns  = [ "state",      key "INTEGER"
                                ; "date_start", key "TIMESTAMP"
                                ; "date_end",   key "TIMESTAMP"
                                ]
                   }

  let keys_streams = { time_key = Some "date"
                     ; columns = [ "streams",  key "TEXT"
                                 ; "date",     key "TIMESTAMP" ~default:"CURRENT_TIMESTAMP"
                                 ]
                     }
                 
  let keys_structs_ts = { time_key = Some "date"
                        ; columns = [ "stream",    key "INTEGER"
                                    ; "structure", key "TEXT"
                                    ; "date",      key "TIMESTAMP"
                                    ]
                        }

  let keys_structs_t2 = { time_key = Some "date"
                        ; columns = [ "stream",    key "INTEGER"
                                    ; "structure", key "TEXT"
                                    ; "date",      key "TIMESTAMP"
                                    ]
                        }

  let keys_bitrate = { time_key = Some "date"
                     ; columns = [ "stream",    key "INTEGER"
                                 ; "bitrates",  key "TEXT"
                                 ; "date",      key "TIMESTAMP"
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
                 
  let tables id =
    let id = string_of_int id in
    let names = { state     = "qos_niit_state_" ^ id
                ; streams   = "qos_niit_streams_" ^ id
                ; struct_ts = "qos_niit_structs_ts_" ^ id
                ; struct_t2 = "qos_niit_structs_t2_" ^ id
                ; bitrate   = "qos_niit_bitrate_" ^ id
                ; errors    = "qos_niit_errors_" ^ id
                }
    in
    names,
    [ names.state, keys_state, None
    ; names.streams, keys_streams, None
    ; names.struct_ts, keys_structs_ts, None
    ; names.struct_t2, keys_structs_t2, None
    ; names.bitrate, keys_bitrate, None
    ; names.errors, keys_errors, None
    ]
           
end

module Conn = Storage.Database.Make(Model)

type t = Conn.t

let unwrap = function Ok v -> v | Error e -> failwith e

let is_in field to_string = function
  | [] -> ""
  | lst -> Printf.sprintf " %s IS IN (%s) AND " field (String.concat "," @@ List.map to_string lst)

module Device = struct

  type state = Common.Topology.state

  let state_to_int = function `Fine -> 0 | `Init -> 1 | `No_response -> 2
  let state_of_int = function 0 -> `Fine | 1 -> `Init | _ -> `No_response

  let init db =
    let open Printf in
    let table = (Conn.names db).state in
    let insert_new = R.exec Types.(tup3 int ptime ptime)
                       (sprintf "INSERT INTO %s (state,date_start,date_end) VALUES (?,?,?)" table) in
    let now = Time.Clock.now_s () in
    let new_state = state_to_int `No_response in
    Conn.request db Request.(exec insert_new (new_state,now,now))
                                                           
  let bump db state =
    let open Printf in
    let table = (Conn.names db).state in
    let select_last = R.find_opt Types.unit Types.(tup3 int ptime ptime)
                        (sprintf "SELECT * FROM %s ORDER BY date_end DESC LIMIT 1" table) in
    let update_last = R.exec Types.(tup3 ptime ptime ptime)
                        (sprintf "UPDATE %s SET date_end = ? WHERE date_start = ? AND date_end = ?" table)
                         (* TODO optimize out*) in
    let insert_new = R.exec Types.(tup3 int ptime ptime)
                       (sprintf "INSERT INTO %s (state,date_start,date_end) VALUES (?,?,?)" table) in
    let now = Time.Clock.now_s () in
    let new_state = state_to_int state in
    Conn.request db Request.(with_trans (find select_last () >>= function
                                         | None -> exec insert_new (new_state,now,now)
                                         | Some (state,st,en) ->
                                            if state = new_state
                                            then exec update_last (now,st,en)
                                            else exec insert_new (new_state,en,now)))
  let max_limit = 500

  let select_state db ?(limit = max_limit) ~from ~till =
    let table = (Conn.names db).state in
    let select = R.collect Types.(tup3 ptime ptime int) Types.(tup3 int ptime ptime)
                  (sprintf {|SELECT * FROM %s WHERE date_start <= $2 AND date_end >= $1 
                            ORDER BY date_end DESC LIMIT $3|} table)
    in Conn.request db Request.(list select (from,till,limit) >>= fun l ->
                                let data = List.map (fun (st,s,e) -> (state_of_int st,s,e)) l in
                                return (Raw { data; has_more = (List.length data >= limit); order = `Desc }))

  let select_state_compressed_internal db ~from ~till =
    let table = (Conn.names db).state in
    let dif   = Time.(Span.to_float_s @@ diff till from) in
    let select_i = R.collect Types.(tup2 ptime ptime) Types.(tup2 int ptime_span)
                     (sprintf {|SELECT state, sum(date_end - date_start) FROM %s 
                               WHERE date_start <= $2 AND date_start >= $1
                               AND date_end <= $2 AND date_end >= $1
                               GROUP BY state|} table) in
    let select_l = R.find_opt Types.(tup2 ptime ptime) Types.(tup2 int ptime)
                     (sprintf {|SELECT state, max(date_end) FROM %s 
                               WHERE date_start < $1 AND date_end <= $2 AND date_end >= $1 
                               GROUP BY state LIMIT 1|} table) in 
    let select_r = R.find_opt Types.(tup2 ptime ptime) Types.(tup2 int ptime)
                     (sprintf {|SELECT state, min(date_start) FROM %s 
                               WHERE date_end > $2 AND date_start <= $2 AND date_start >= $1 
                               GROUP BY state LIMIT 1|} table) in
    let select_o = R.find_opt Types.(tup2 ptime ptime) Types.int
                     (sprintf {|SELECT state FROM %s 
                               WHERE date_end > $2 AND date_start < $1 
                               GROUP BY state LIMIT 1|} table) in
    Conn.request db Request.(find select_o (from,till) >>= function
                             | Some s ->
                                return (match state_of_int s with
                                        | `Fine -> (100.,0.,0.)
                                        | `Init -> (0.,100.,0.)
                                        | `No_response -> (0.,0.,100.))
                             | None ->
                                find select_l (from,till) >>= fun l ->
                                find select_r (from,till) >>= fun r ->
                                list select_i (from,till) >>= fun i ->
                                let l = match l with
                                  | None -> fun x -> x
                                  | Some (st,t) ->
                                     let sp = Time.diff t from in
                                     fun (s,v) -> if Pervasives.(=) s st then (s,Time.Span.add sp v)
                                                  else (s,v) in
                                let r = match r with
                                  | None -> fun x -> x
                                  | Some (st,t) ->
                                     let sp = Time.diff till t in
                                     fun (s,v) -> if s = st then (s,Time.Span.add sp v)
                                                  else (s,v) in
                                let i = List.map (fun x ->
                                            let st, span = r @@ l @@ x in
                                            let st  = state_of_int st in
                                            let per = 100. *. (Time.Span.to_float_s span) /. dif in
                                            st,per) i in
                                let res = List.fold_left (fun (f,i,n) -> function
                                              | `Fine, x -> (f +. x, i, n)
                                              | `Init, x -> (f, i +. x, n)
                                              | `No_response, x -> (f, i, n +. x)) (0.,0.,0.) i
                                in return res )

  let select_state_compressed db ~from ~till =
    select_state_compressed_internal db ~from ~till
    >|= fun data -> Compressed { data }
    
end
       
module Streams = struct

  type state = Common.Topology.state

  let insert_streams db streams =
    let table  = (Conn.names db).streams in
    let data   = Yojson.Safe.to_string @@ Common.Json.List.to_yojson Common.Stream.to_yojson streams in
    let insert = R.exec Types.string
                   (sprintf "INSERT INTO %s (streams) VALUES (?)" table)
    in Conn.request db Request.(exec insert data)
     
  let select_streams db ?(with_pre = true) ?(limit = 500) ~from ~till =
    let table  = (Conn.names db).streams in
    let select = R.collect Types.(tup3 ptime ptime int) Types.(tup2 string ptime)
                   (sprintf "SELECT * FROM %s WHERE date >= $1 AND date <= $2 ORDER BY date DESC LIMIT $3" table) in
    let select_pre = R.collect Types.(tup3 ptime ptime int) Types.(tup2 string ptime)
                       (sprintf {|(SELECT * FROM %s WHERE date >= $1 AND date <= $2 ORDER BY date DESC)
                                 UNION ALL
                                 (SELECT * FROM %s WHERE date < $1 ORDER BY date DESC LIMIT 1) 
                                 ORDER BY date LIMIT $3|} table table) in
    Conn.request db Request.(list (if with_pre then select_pre else select) (from, till, limit) >>= fun l ->
                             try let data = List.map (fun (s,t) -> Result.get_exn
                                                                   @@ Common.Json.List.of_yojson Common.Stream.of_yojson
                                                                   @@ Yojson.Safe.from_string s,
                                                                   t) l
                                 in return @@ Ok (Raw { data; has_more = List.length data >= limit; order = `Desc })
                             with _ -> return @@ Error "select_streams failure")

  let insert_structs_ts db structs =
    let table  = (Conn.names db).struct_ts in
    let data   = List.map (fun (id,st) -> Common.Stream.id_to_int32 id,
                                          Yojson.Safe.to_string @@ Board_types.Streams.TS.structure_to_yojson st,
                                          st.timestamp)
                   structs in
    let insert = R.exec Types.(tup3 int32 string ptime)
                   (sprintf "INSERT INTO %s (stream,structure,date) VALUES (?,?,?)" table)
    in Conn.request db Request.(with_trans (List.fold_left (fun acc v -> acc >>= fun () -> exec insert v)
                                              (return ()) data))
     
  let select_structs_ts db ?(with_pre = true) ?(limit = 500) ?(ids = []) ~from ~till =
    let table  = (Conn.names db).struct_ts in
    let ids    = is_in "stream" Int32.to_string ids in
    let select = R.collect Types.(tup3 ptime ptime int) Types.(tup3 int32 string ptime)
                   (sprintf {|SELECT * FROM %s WHERE %s date >= $1 AND date <= $2 
                             ORDER BY date DESC LIMIT $3|} table ids)
    in
    let select_pre =
      R.collect Types.(tup3 ptime ptime int) Types.(tup3 int32 string ptime)
        (sprintf {|(SELECT * FROM %s WHERE %s date >= $1 AND date <= $2 ORDER BY date DESC)
                  UNION ALL
                  (SELECT * FROM %s WHERE %s date < $1 ORDER BY date DESC LIMIT 1) 
                  ORDER BY date LIMIT $3|} table ids table ids) in
    Conn.request db
      Request.(list (if with_pre then select_pre else select)
                 (from, till, limit)
               >>= fun l ->
               try let data =
                     List.map (fun (id,s,_) ->
                         Common.Stream.id_of_int32 id,
                         unwrap
                         @@ Board_types.Streams.TS.structure_of_yojson
                         @@ Yojson.Safe.from_string s) l
                   in return @@ Ok (Raw { data
                                        ; has_more = List.length data >= limit
                                        ; order    = `Desc })
               with e -> return @@ Error (Printexc.to_string e))

  let insert_structs_t2 db structs =
    let table  = (Conn.names db).struct_t2 in
    let data   = List.map (fun (id,st) -> id,
                                          Yojson.Safe.to_string @@ Board_types.Streams.T2MI.structure_to_yojson st,
                                          st.timestamp)
                   structs in
    let insert = R.exec Types.(tup3 int string ptime)
                   (sprintf "INSERT INTO %s (stream,structure,date) VALUES (?,?,?)" table)
    in Conn.request db Request.(with_trans (List.fold_left (fun acc v -> acc >>= fun () -> exec insert v)
                                              (return ()) data))
     
  let select_structs_t2 db ?(with_pre = true) ?(limit = 500) ?(ids = []) ~from ~till =
    let table  = (Conn.names db).struct_t2 in
    let ids    = is_in "stream" string_of_int ids in
    let select = R.collect Types.(tup3 ptime ptime int) Types.(tup3 int string ptime)
                   (sprintf {|SELECT * FROM %s WHERE %s date >= $1 AND date <= $2 
                             ORDER BY date DESC LIMIT $3|} table ids)
    in
    let select_pre = R.collect Types.(tup3 ptime ptime int) Types.(tup3 int string ptime)
                       (sprintf {|(SELECT * FROM %s WHERE %s date >= $1 AND date <= $2 ORDER BY date DESC)
                                 UNION ALL
                                 (SELECT * FROM %s WHERE %s date < $1 ORDER BY date DESC LIMIT 1) 
                                 ORDER BY date LIMIT $3|} table ids table ids) in
    Conn.request db Request.(list (if with_pre then select_pre else select) (from, till, limit) >>= fun l ->
                             try let data = List.map (fun (id,s,t) -> id,
                                                                      unwrap
                                                                      @@ Board_types.Streams.T2MI.structure_of_yojson
                                                                      @@ Yojson.Safe.from_string s,
                                                                      t) l
                                 in return @@ Ok (Raw { data; has_more = List.length data >= limit; order = `Desc })
                             with e -> return @@ Error (Printexc.to_string e))

  let insert_bitrate db bits =
    let table  = (Conn.names db).bitrate in
    let data   = List.map (fun (id,bit) -> Common.Stream.id_to_int32 id,
                                          Yojson.Safe.to_string @@ Board_types.Streams.TS.bitrate_to_yojson bit,
                                          bit.timestamp)
                   bits in
    let insert = R.exec Types.(tup3 int32 string ptime)
                   (sprintf "INSERT INTO %s (stream,bitrates,date) VALUES (?,?,?)" table)
    in Conn.request db Request.(with_trans (List.fold_left (fun acc v -> acc >>= fun () -> exec insert v)
                                              (return ()) data))
  
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
    let open Printf in
    let table = (Conn.names db).errors in
    let errs = List.map (fun (s,e) -> List.map (Pair.make s) e) errs |> List.concat in
    let insert =
      R.exec Types.(tup2 bool error)
        (sprintf {|INSERT INTO %s(is_ts,stream,count,err_code,err_ext,priority,
                  multi_pid,pid,packet,param_1,param_2,date)
                  VALUES (?,?,?,?,?,?,?,?,?,?,?,?)|} table)
    in Conn.request db Request.(with_trans (List.fold_left (fun acc x -> acc >>= fun () -> exec insert (is_ts,x))
                                              (return ()) errs))

  let select_has_any db ?(streams = []) ?(priority = []) ?(pids = []) ?(errors = []) ~is_ts ~from ~till () =
    let open Printf in
    let table = (Conn.names db).errors in
    let streams  = is_in "stream" Int32.to_string streams in
    let priority = is_in "priority" string_of_int priority in
    let pids     = is_in "pid" string_of_int pids in
    let errors   = is_in "err_code"  string_of_int errors in
    let select = R.find Types.(tup3 bool ptime ptime) Types.bool
                   (sprintf {|SELECT TRUE FROM %s
                             WHERE %s %s %s %s is_ts = ? AND date >= ? AND date <= ? LIMIT 1|}
                      table streams priority pids errors)
    in Conn.request db Request.(find select (is_ts,from,till) >>= function None -> return false | Some x -> return x)

  let select_percent db ?(streams = []) ?(priority = []) ?(pids = []) ?(errors = []) ~is_ts ~from ~till () =
    let open Printf in
    let table = (Conn.names db).errors in
    let streams  = is_in "stream" Int32.to_string streams in
    let priority = is_in "priority" string_of_int priority in
    let pids     = is_in "pid" string_of_int pids in
    let errors   = is_in "err_code"  string_of_int errors in
    let span = Time.(Period.to_float_s @@ diff till from) in
    let select = R.find Types.(tup3 bool ptime ptime) Types.int
                   (sprintf {|SELECT count(DISTINCT date_trunc('second',date)) FROM %s 
                             WHERE %s %s %s %s is_ts = ? AND date >= ? AND date <= ?|}
                      table streams priority pids errors)
    in Conn.request db Request.(find select (is_ts,from,till) >>= function
                                | None -> return 0.0
                                | Some s -> return (100. *. (float_of_int s) /. span))
                
  let select_errors db ?(streams = []) ?(priority = []) ?(pids = []) ?(errors = []) ?(limit = 500)
        ~is_ts ~from ~till () =
    let open Printf in
    let table    = (Conn.names db).errors in
    let streams  = is_in "stream" Int32.to_string streams in
    let priority = is_in "priority" string_of_int priority in
    let pids     = is_in "pid" string_of_int pids in
    let errors   = is_in "err_code"  string_of_int errors in
    let select = R.collect Types.(tup4 bool ptime ptime int) error
                   (sprintf {|SELECT stream,count,err_code,err_ext,priority,
                             multi_pid,pid,packet,param_1,param_2,date FROM %s 
                             WHERE %s %s %s %s is_ts = ? AND date >= ? AND date <= ?
                             ORDER BY date DESC LIMIT ?|}
                      table streams priority pids errors)
    in Conn.request db Request.(list select (is_ts,from,till,limit) >>= fun data ->
                                return (Raw { data; has_more = (List.length data >= limit); order = `Desc }))
     
  let select_errors_compressed db ?(streams = []) ?(priority = []) ?(pids = []) ?(errors = []) ~is_ts ~from ~till () =
    let open Printf in
    let table    = (Conn.names db).errors in
    let streams  = is_in "stream" Int32.to_string streams in
    let priority = is_in "priority" string_of_int priority in
    let pids     = is_in "pid" string_of_int pids in
    let errors   = is_in "err_code"  string_of_int errors in
    let intvals = Time.split ~from ~till in
    let select = R.find Types.(tup3 bool ptime ptime ) Types.int
                   (sprintf {|SELECT count(DISTINCT date_trunc('second',date)) FROM %s
                             WHERE %s %s %s %s is_ts = ? AND date >= ? AND date <= ?|}
                      table streams priority pids errors)
    in Conn.request db Request.(with_trans (List.fold_left (fun acc (f,t) ->
                                                let span = Time.(Period.to_float_s @@ diff till from) in
                                                acc >>= fun l ->
                                                find select (is_ts, f, t) >>= function
                                                | None -> return l
                                                | Some s -> let perc = (100. *. (float_of_int s) /. span) in
                                                            return ((perc,f,t)::l))
                                              (return []) intvals)
                                >>= return)
       >>= fun l ->
       List.fold_left (fun acc (p,from,till) ->
           acc >>= fun acc ->
           Device.select_state_compressed_internal db ~from ~till
           >>= fun (f,_,_) ->
           Lwt.return ( { errors    = (100. *. p /. f)
                        ; no_stream = (100. -. f)
                        ; period    = from, till } :: acc))
         (Lwt.return []) l
       >|= fun data -> Compressed { data }

end
