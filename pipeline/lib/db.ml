open Containers
open Storage.Database
open Common
open Qoe_errors
open Api.Api_types
   
open Lwt.Infix

module SID = struct

  type t = Stream.ID.t
  let db_type : string = "UUID"
  let of_stream_id (id:Stream.ID.t) =
    Stream.ID.to_string id 
  let to_stream_id (t:string) =
    Stream.ID.of_string t
  let typ     = Caqti_type.custom
                  ~encode:(fun x -> Ok (of_stream_id x))
                  ~decode:(fun x -> try Ok (to_stream_id x) with _ -> Error "Bad SID")
                  Caqti_type.string
end
   
let data_t =
  Caqti_type.custom
    Caqti_type.(let (&) = tup2 in
                SID.typ & int & int & int
                & int & int
                & float & float & float
                & bool & bool & ptime)
    ~encode:(fun (stream,channel,pid,error,data) ->
      let (&) a b = (a, b) in
      Ok(stream & channel & pid & error & data.counter & data.size
         & data.params.min & data.params.max & data.params.avg 
         & data.peak_flag & data.cont_flag & data.timestamp))
    ~decode:(fun (stream,(channel,(pid,(error,(counter,(size,(min,(max,(avg,(peak_flag,(cont_flag,timestamp))))))))))) ->
      Ok(stream,channel,pid,error, { counter; size; params = { min; max; avg }; peak_flag;
                                     cont_flag; timestamp }))
                                

(* TODO testing *)
  
module Model = struct
  open Key_t

  type init = ()

  type names = { streams     : string
               ; pid_state   : string
               ; structs     : string
               ; stream_loss : string
               ; errors      : string
               }
            
  let name = "qoe(pipeline)"

  let streams_keys =
    make_keys ~time_key:"date_end"
      [ "streams",    key "JSONB"
      ; "date_start", key "TIMESTAMP"
      ; "date_end",   key "TIMESTAMP"
      ]

  let pid_state_keys = { time_key = Some "date_end"
                       ; columns  = [ "stream",     key SID.db_type
                                    ; "channel",    key "INTEGER"
                                    ; "pid",        key "INTEGER"
                                    ; "date_start", key "TIMESTAMP"
                                    ; "date_end",   key "TIMESTAMP"
                                    ]
                       }
           
  let struct_keys  = { time_key = Some "date"
                     ; columns  = [ "id",      key "JSONB"
                                  ; "struct",  key "JSONB"
                                  ; "date",    key "TIMESTAMP" ~default:"CURRENT_TIMESTAMP"
                                  ]
                     }

  let stream_loss_keys = { time_key = Some "date_end"
                         ; columns  = [ "stream",     key SID.db_type
                                      ; "channel",    key "INTEGER"
                                      ; "pid",        key "INTEGER"
                                      ; "date_start", key "TIMESTAMP"
                                      ; "date_end",   key "TIMESTAMP"
                                      ]
                         }

  let err_keys = { time_key = Some "date"
                 ; columns = [ "stream",  key SID.db_type
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
        
  let tables () =
    let names = { streams = "qoe_streams"
                ; pid_state = "qoe_pid_state"
                ; structs = "qoe_structures"
                ; stream_loss = "qoe_stream_loss"
                ; errors = "qoe_errors" } in
    names,
    [ (names.streams, streams_keys, None)
    ; (names.pid_state, pid_state_keys, None)
    ; (names.structs, struct_keys, None)
    ; (names.stream_loss, stream_loss_keys, None)
    ; (names.errors, err_keys, None)
    ]
end

module Conn = Storage.Database.Make(Model)

module R = Caqti_request

let is_in field to_string = function
  | [] -> ""
  | lst -> Printf.sprintf " %s IN (%s) AND " field (String.concat "," @@ List.map to_string lst)

let merge_intervals =
  let (<=) l r = Time.compare l r <= 0 in
  let (>=) l r = Time.compare l r >= 0 in
  let join l r = List.fold_left (fun acc (f,t) ->
                     List.append acc @@ List.filter_map
                                          (fun (ff,tt) ->
                                            if f <= ff && t >= tt then Some (ff,tt)
                                            else if f <=ff && t >= ff then Some (ff, t)
                                            else if f <= tt && t >= tt then Some (f, tt)
                                            else if f >= ff && t <= tt then Some (f, t)
                                            else None) r) [] l
  in (* TODO add compress *) 
  join
         
module Streams = struct

  let init db streams =
    let open Printf in
    let table = (Conn.names db).streams in
    let insert_new = R.exec Types.(tup3 string ptime ptime)
                       (sprintf "INSERT INTO %s (streams,date_start,date_end) VALUES (?,?,?)" table) in
    let now = Ptime_clock.now () in
    let streams = Yojson.Safe.to_string @@ Common.(Json.List.to_yojson Stream.to_yojson streams) in
    Conn.request db Request.(exec insert_new (streams,now,now))

  let bump db =
    let open Printf in
    let table = (Conn.names db).streams in
    let update_last = R.exec Types.ptime
                        (sprintf {|UPDATE %s SET date_end = ? WHERE 
                                  date_start = (SELECT date_start FROM %s ORDER BY date_start DESC LIMIT 1)|}
                           table table)
    (* TODO optimize out*) in
    let now = Ptime_clock.now () in
    Conn.request db Request.(exec update_last now)

  let select_streams db ?(limit = 500) ~from ~till =
    let open Printf in
    let table = (Conn.names db).streams in
    let select = R.collect Types.(tup3 ptime ptime int) Types.(tup3 string ptime ptime)
                   (sprintf {|SELECT * FROM %s WHERE date_end >= ? AND date_start <= ? 
                             ORDER BY date_end LIMIT ?|} table) in
    Conn.request db Request.(list select (from,till,limit) >>= fun l ->
                             try let data = List.map (fun (s,f,t) ->
                                                match Stream.of_yojson @@ Yojson.Safe.from_string s with
                                                | Ok v -> (v,f,t)
                                                | Error e -> failwith e) l
                                 in return (Ok (Raw { data
                                                    ; has_more = (limit <= List.length data)
                                                    ; order = `Desc }))

                             with Failure e -> return (Error e))
  
end

module Pid_state = struct

  let init db pids =
    let open Printf in
    let table = (Conn.names db).pid_state in
    let insert_new = R.exec Types.(tup4 SID.typ int int (tup2 ptime ptime))
                       (sprintf "INSERT INTO %s (stream,channel,pid,date_start,date_end) VALUES (?,?,?,?,?)" table) in
    let now = Ptime_clock.now () in
    Logs.err (fun m -> m "DB pids len: %d" @@ List.length pids);
    Conn.request db Request.(with_trans (List.fold_left (fun acc (s,c,p,_) ->
                                             acc >>= fun () -> exec insert_new (s,c,p,(now,now)))
                                           (return ()) pids))
    
  let bump db pids =
     let open Printf in
     let table = (Conn.names db).pid_state in
     let update_last = R.exec Types.(tup4 SID.typ int int ptime)
                         (sprintf {|UPDATE %s SET date_end = $4
                                   WHERE stream = $1 AND channel = $2 AND pid = $3
                                   AND date_start = (SELECT date_start FROM %s 
                                                     WHERE stream = $1 AND channel = $2 AND pid = $3 
                                                     ORDER BY date_start DESC LIMIT 1)|}
                            table table)
     in
     let now = Ptime_clock.now () in
     Conn.request db Request.(with_trans (List.fold_left (fun acc (s,c,p,_) ->
                                              acc >>= fun () -> exec update_last (s,c,p,now))
                                            (return ()) pids))

  let req_select_distinct_pids db ~from ~till =
    let open Printf in
    let table = (Conn.names db).pid_state in
    let r = R.collect Types.(tup2 ptime ptime) Types.(tup3 SID.typ int int)
              (sprintf {|SELECT DISTINCT stream,channel,pid FROM %s 
                        WHERE date_start <= $2 AND date_end >= $1|} table)
    in Request.(list r (from,till))

  let req_select_intervals db ?(limit = 500) ~stream ~channel ~pid ~from ~till =
    let open Printf in
    let table = (Conn.names db).pid_state in
    let r = R.collect Types.(tup4 SID.typ int int (tup3 ptime ptime int)) Types.(tup2 ptime ptime)
              (sprintf {|SELECT date_start,date_end FROM %s 
                        WHERE stream = $1 AND channel = $2 AND pid = $3
                        AND date_end >= $4 AND date_start <= $5 ORDER BY date_end DESC LIMIT $6|} table)
    in Request.(list r (stream,channel,pid,(from,till,limit)))

  let req_select_span db ~stream ~channel ~pid ~from ~till =
    let open Printf in
    let table = (Conn.names db).pid_state in
    let full  = Time.diff till from in
    let r = R.find Types.(tup4 SID.typ int int (tup2 ptime ptime)) Types.ptime_span
              (sprintf {|SELECT SUM(date_end - date_start) FROM
                        ((SELECT date_start,date_end FROM %s 
                          WHERE stream = $1 AND channel = $2 AND pid = $3 AND date_start >= $4 AND date_end <= $5)
                         UNION
                         (SELECT $4 as date_start, date_end FROM %s
                          WHERE stream = $1 AND channel = $2 AND pid = $3 
                          AND date_start < $4 AND date_end <= $5 AND date_end >= $4
                          ORDER BY date_end DESC LIMIT 1)
                         UNION
                         (SELECT date_start, $5 as date_end FROM %s
                          WHERE stream = $1 AND channel = $2 AND pid = $3 
                          AND date_end > $5 AND date_start <= $5 AND date_start >= $1
                          ORDER BY date_end DESC LIMIT 1)
                         UNION
                         (SELECT $4 as date_start, $5 as date_end FROM %s
                          WHERE stream = $1 AND channel = $2 AND pid = $3 
                          AND date_start < $4 AND date_end > $5
                          ORDER BY date_end DESC LIMIT 1)) as temp|} table table table table)
    in Request.(find r (stream,channel,pid,(from,till)) >>= function
                | Some span ->
                   if Time.Period.compare full span < 0 then return full else return span
                | None -> return full)
     
end
         
module Structure = struct
  let insert_structures db streams : unit Lwt.t =
    let open Printf in
    let table   = (Conn.names db).structs in
    let entries = Structure_conv.dump_structures streams in
    let insert  = R.exec Types.(tup2 string string)
                    (sprintf "INSERT INTO %s(id, struct) VALUES (?,?)" table)
    in Conn.request db Request.(with_trans (List.fold_left (fun acc e -> acc >>= fun () -> exec insert e)
                                              (return ()) entries))

  let select_structures db ?(uris=[]) ?(limit=500) ~from ~till =
    let open Printf in
    let table   = (Conn.names db).structs in
    let uris    = is_in "id" (fun x -> Yojson.Safe.to_string
                                       @@ Stream.container_id_to_yojson (TSoIP x)) uris in
    let select = 
      R.collect Types.(tup3 ptime ptime int) Caqti_type.(tup2 string ptime)
        (sprintf {|(SELECT structs, date FROM %s WHERE %s date > $1 AND date <= $2)
                  UNION
                  (SELECT structs, date FROM %s WHERE %s date < $1 ORDER BY date DESC LIMIT 1)
                  ORDER BY date DESC LIMIT $3|} table uris table uris)
    in Conn.request db Request.(list select (from,till,limit) >>= fun l ->
                                try let data = List.map (fun (s,t) ->
                                                   match Structure.structure_of_yojson @@ Yojson.Safe.from_string s with
                                                   | Ok v -> v,t
                                                   | Error e -> failwith e) l
                                    in return (Ok (Raw { data; has_more = List.length data >= limit; order = `Desc }))
                                with Failure e -> return (Error e))

end

module Stream_status = struct

  let init db pids =
    let open Printf in
    let open Qoe_status in
    let table = (Conn.names db).stream_loss in
    let insert_new = R.exec Types.(tup4 SID.typ int int (tup2 ptime ptime))
                       (sprintf "INSERT INTO %s (stream,channel,pid,date_start,date_end) VALUES (?,?,?,?,?)" table) in
    let now = Ptime_clock.now () in
    Logs.err (fun m -> m "DB pids len: %d" @@ List.length pids);
    Conn.request db Request.(with_trans (List.fold_left (fun acc {stream;channel;pid;_} ->
                                             acc >>= fun () ->
                                             exec insert_new (stream,channel,pid,(now,now)))
                                           (return ()) pids))
    
  let bump db pids =
    let open Printf in
    let open Qoe_status in
    let table = (Conn.names db).stream_loss in
    let update_last = R.exec Types.(tup4 SID.typ int int ptime)
                        (sprintf {|UPDATE %s SET date_end = $4
                                  WHERE stream = $1 AND channel = $2 AND pid = $3
                                  AND date_start = (SELECT date_start FROM %s 
                                  WHERE stream = $1 AND channel = $2 AND pid = $3 
                                  ORDER BY date_start DESC LIMIT 1)|}
                           table table)
    in
    let now = Ptime_clock.now () in
    Conn.request db Request.(with_trans (List.fold_left (fun acc {stream;channel;pid;_} ->
                                             acc >>= fun () ->
                                             exec update_last (stream,channel,pid,now))
                                           (return ()) pids))
     
end 

module Errors = struct
  let insert_data db data =
    let open Printf in
    let table   = (Conn.names db).errors in
    let insert =
      R.exec data_t
        (sprintf {|INSERT INTO %s(stream,channel,pid,error,counter,size,min,max,avg,peak_flag,cont_flag,date)
                  VALUES (?,?,?,?,?,?,?,?,?,?,?,?)|} table)
    in Conn.request db Request.(with_trans (List.fold_left (fun acc x -> acc >>= fun () -> exec insert x)
                                              (return ()) data))

  let insert_audio db data = insert_data db (audio_data_to_list data)

  let insert_video db data = insert_data db (video_data_to_list data)

  let coords_conv = is_in "(stream,channel,pid)" (fun (s,c,p) -> Printf.sprintf "(%s,%d,%d)"
                                                                   (SID.of_stream_id s) c p)
                           
  let select_has_any db ?(coords = []) ?(errors = []) ?(typ = `Both) ~from ~till =
    let open Printf in
    let table   = (Conn.names db).errors in
    let errors   = is_in "error" string_of_int (List.map labels_to_int errors) in
    let coords   = coords_conv coords in
    let typ      = match typ with
      | `Both -> "(peak_flag = TRUE OR cont_flag = TRUE) AND"
      | `Peak -> "peak_flag = TRUE AND"
      | `Cont -> "cont_flag = TRUE AND"
    in
    let select = R.find Types.(tup2 ptime ptime) Types.bool
                   (sprintf {|SELECT EXISTS (SELECT 1 FROM %s
                             WHERE %s %s %s date >= ? AND date <= ?)|}
                      table typ errors coords)
    in Conn.request db Request.(find select (from,till) >>= function
                                | None   -> return false
                                | Some x -> return x)

  let select_percent db ?(coords = []) ?(errors = []) ?(typ = `Both) ~from ~till =
    let open Printf in
    let table   = (Conn.names db).errors in
    let errors   = is_in "error"   string_of_int (List.map labels_to_int errors) in
    let typ      = match typ with
      | `Both -> "(peak_flag = TRUE OR cont_flag = TRUE) AND"
      | `Peak -> "peak_flag = TRUE AND"
      | `Cont -> "cont_flag = TRUE AND"
    in
    let select = R.find Types.(tup4 SID.typ int int (tup2 ptime ptime)) Types.int
                   (sprintf {|SELECT count(DISTINCT date_trunc('second',date)) FROM %s 
                             WHERE %s %s stream = $1 AND channel = $2 AND pid = $3 AND date >= $4 AND date <= $5|}
                      table typ errors)
    in Conn.request db Request.(begin match coords with
                                | [] -> Pid_state.req_select_distinct_pids db ~from ~till
                                | ls -> return ls
                                end >>= fun coords ->
                                with_trans (List.fold_left (fun acc (s,c,p) ->
                                                acc >>= fun acc ->
                                                (Pid_state.req_select_span db ~stream:s ~channel:c ~pid:p ~from ~till)
                                                >>= fun span ->
                                                let span = Time.Period.to_float_s span in
                                                find select (s,c,p,(from,till)) >>= function
                                                | None   -> return ((s,c,p,0.0)::acc)
                                                | Some x ->
                                                   let perc = (100. *. (float_of_int x) /. span) in
                                                   return ((s,c,p,perc)::acc))
                                              (return []) coords))

  let select_errors db ?(limit = 500) ?(coords = []) ?(errors = []) ?(typ = `Both) ~from ~till =
    let open Printf in
    let table    = (Conn.names db).errors in
    let errors   = is_in "error"   string_of_int (List.map labels_to_int errors) in
    let coords   = coords_conv coords in
    let typ      = match typ with
      | `Both -> "(peak_flag = TRUE OR cont_flag = TRUE) AND"
      | `Peak -> "peak_flag = TRUE AND"
      | `Cont -> "cont_flag = TRUE AND"
    in
    let select = R.collect Types.(tup3 ptime ptime int) data_t
                   (sprintf {|SELECT stream,channel,pid,error,counter,size,min,max,avg,peak_flag,cont_flag,date
                             FROM %s WHERE %s %s %s date >= ? AND date <= ?
                             ORDER BY date DESC LIMIT ?|}
                      table typ errors coords)
    in Conn.request db Request.(list select (from,till,limit) >>= fun data ->
                                return (Raw { data; has_more = (List.length data >= limit); order = `Desc }))

  let select_errors_compressed db ?(errors = []) ?(typ = `Both) ~coords ~from ~till =
    let open Printf in
    let table    = (Conn.names db).errors in
    let errors   = is_in "error"   string_of_int errors in
    let typ      = match typ with
      | `Both -> "(peak_flag = TRUE OR cont_flag = TRUE) AND"
      | `Peak -> "peak_flag = TRUE AND"
      | `Cont -> "cont_flag = TRUE AND"
    in
    let intvals = Time.split ~from ~till in
    let select = R.find Types.(tup4 SID.typ int int (tup2 ptime ptime)) Types.int
                   (sprintf {|SELECT count(DISTINCT date_trunc('second',date)) FROM %s
                             WHERE %s %s stream = $1 AND channel = $2 AND pid = $3 AND date >= $4 AND date <= $5|}
                      table typ errors)
    in Conn.request db Request.(
      with_trans (List.fold_left (fun acc (stream,channel,pid) -> 
                      let data = List.fold_left (fun acc (from,till) ->
                                     acc >>= fun acc ->
                                     Pid_state.req_select_span db ~stream ~channel ~pid ~from ~till
                                     >>= fun period ->
                                     let span   = Time.(Period.to_float_s @@ diff till from) in
                                     let period = Time.Period.to_float_s period in
                                     let no_conn, period  =
                                       if Float.(period >= span)
                                       then 0.0, span
                                       else span -. period, period
                                     in
                                     find select (stream,channel,pid,(from, till)) >>= function
                                     | None -> return acc
                                     | Some s -> let perc = (100. *. (float_of_int s) /. period) in
                                                 return ((perc,no_conn,from,till)::acc))
                                   (return []) intvals
                      in acc  >>= fun acc ->
                         data >>= fun data ->
                         return ((stream, channel, pid, List.rev data)::acc))
                    (return []) coords)
      >>= fun data -> return @@ Compressed { data })
      
     
  let select_states db ?(limit = 500) ~error ~stream ~channel ~pid ~from ~till =
    let open Printf in
    let table    = (Conn.names db).errors in
    let select = R.collect Types.(List.(SID.typ & int & int & int & ptime & ptime & int)) Types.(tup4 float float float ptime)
                   (sprintf {|SELECT min,max,avg,date FROM %s 
                             WHERE stream = $1 AND channel = $2 AND pid = $3 AND error = $4
                             AND date >= $5 AND date <= $6 ORDER BY date DESC LIMIT $7|}
                      table)
    in Conn.request db Request.(list select (stream,(channel,(pid,(error,(from,(till,limit)))))) >>= fun data ->
                                return @@ Raw { data; has_more = List.length data >= limit; order = `Desc })
     
  (* TODO reconsider avg *)
  let select_states_compress db ~error ~stream ~channel ~pid ~from ~till =
    let open Printf in
    let table    = (Conn.names db).errors in
    let intvals  = Time.split ~from ~till in
    let select   = R.find Types.(List.(SID.typ & int & int & int & ptime & ptime)) Types.(tup3 float float float)
                   (sprintf {|SELECT min(min),max(max),sum(avg)/count(1) FROM %s 
                             WHERE stream = $1 AND channel = $2 AND pid = $3 AND error = $4
                             AND date >= $5 AND date <= $6|}
                      table)
    in Conn.request db Request.(
      with_trans (List.fold_left (fun acc (from,till) ->
                      acc >>= fun acc ->
                      find select (stream,(channel,(pid,(error,(from,till))))) >>= function
                      | Some (min,max,avg) -> return ((min,max,avg,from,till)::acc)
                      | None -> return acc)
                    (return []) intvals)
      >>= fun data ->
      return @@ Compressed { data = List.rev data })

end
