open Containers
open Storage.Database
open Api.Api_types
open Board_types
open Lwt.Infix
open Common
open Printf

module R = Caqti_request

module ID : sig

  type t = Stream.ID.t
  type db
  val db : db Caqti_type.t
  val typ : string
  val of_db : db -> t
  val to_db : t -> db
  val to_value_string : t -> string

end = struct

  type t = Stream.ID.t
  type db = string

  let typ : string = "UUID"
  let db : db Caqti_type.t = Types.string
  let to_db (id:t) : db =
    Stream.ID.to_string id
  let of_db (db:db) : t =
    Stream.ID.of_string db
  let to_value_string x =
    let s = Stream.ID.to_string x in
    Printf.sprintf "'%s'::%s" s typ

end

module Model = struct
  open Key_t

  type init = int
  type names =
    { state     : string
    ; streams   : string
    ; ts_info   : string
    ; services  : string
    ; tables    : string
    ; pids      : string
    ; t2mi_info : string
    ; bitrate   : string
    ; errors    : string
    }

  let name = "qos_niit"

  let keys_state =
    { time_key = Some "date_end"
    ; columns  = [ "state",      key "INTEGER"
                 ; "date_start", key "TIMESTAMP"
                 ; "date_end",   key "TIMESTAMP"
                 ]
    }

  let keys_streams =
    { time_key = Some "date_end"
    ; columns  = [ "stream",     key "JSONB"
                 ; "id",         key ID.typ
                 ; "type",       key "TEXT"
                 ; "input",      key "JSONB"
                 ; "date_start", key "TIMESTAMP"
                 ; "date_end",   key "TIMESTAMP"
                 ]
    }

  let keys_ts_info =
    { time_key = Some "date"
    ; columns  = [ "stream", key ID.typ
                 ; "data",   key "TEXT"
                 ; "date",   key "TIMESTAMP"
                 ]
    }

  let keys_services =
    { time_key = Some "date"
    ; columns  = [ "stream", key ID.typ
                 ; "data",   key "TEXT"
                 ; "date",   key "TIMESTAMP"
                 ]
    }

  let keys_tables =
    { time_key = Some "date"
    ; columns  = [ "stream", key ID.typ
                 ; "data",   key "TEXT"
                 ; "date",   key "TIMESTAMP"
                 ]
    }

  let keys_pids =
    { time_key = Some "date"
    ; columns  = [ "stream", key ID.typ
                 ; "data",   key "TEXT"
                 ; "date",   key "TIMESTAMP"
                 ]
    }

  let keys_t2mi_info =
    { time_key = Some "date"
    ; columns  = [ "stream", key ID.typ
                 ; "data",   key "TEXT"
                 ; "date",   key "TIMESTAMP"
                 ]
    }

  let keys_bitrate =
    { time_key = Some "date"
    ; columns  = [ "stream", key ID.typ
                 ; "data",   key "TEXT"
                 ; "date",   key "TIMESTAMP"
                 ]
    }

  let keys_errors =
    { time_key = Some "date"
    ; columns  = [ "is_ts",     key "BOOL"
                 ; "stream",    key ID.typ
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
    let names =
      { state     = "qos_niit_state_"     ^ id
      ; streams   = "qos_niit_streams_"   ^ id
      ; ts_info   = "qos_niit_ts_info_"   ^ id
      ; services  = "qos_niit_services_"  ^ id
      ; tables    = "qos_niit_tables_"    ^ id
      ; pids      = "qos_niit_pids_"      ^ id
      ; t2mi_info = "qos_niit_t2mi_info_" ^ id
      ; bitrate   = "qos_niit_bitrate_"   ^ id
      ; errors    = "qos_niit_errors_"    ^ id
      }
    in
    names,
    [ names.state,     keys_state,     None
    ; names.streams,   keys_streams,   None
    ; names.ts_info,   keys_ts_info,   None
    ; names.services,  keys_services,  None
    ; names.tables,    keys_tables,    None
    ; names.pids,      keys_pids,      None
    ; names.t2mi_info, keys_t2mi_info, None
    ; names.bitrate,   keys_bitrate,   None
    ; names.errors,    keys_errors,    None
    ]

end

module Conn = Storage.Database.Make(Model)

type t = Conn.t

let unwrap = function Ok v -> v | Error e -> failwith e

let is_in field to_string = function
  | [] -> ""
  | lst -> Printf.sprintf " %s IN (%s) AND " field (String.concat "," @@ List.map to_string lst)

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

  (* TODO fix it *)
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
    let now    = Time.Clock.now_s () in
    let data   =
      List.map (fun s ->
          Yojson.Safe.to_string @@ Stream.to_yojson s,
          (ID.to_db s.id, Stream.typ_to_string s.typ),
          Yojson.Safe.to_string
          @@ Topology.topo_input_to_yojson
          @@ Option.get_exn @@ Stream.get_input s, (* FIXME make optional in table *)
          (now, now)) streams in
    let insert =
      R.exec Types.(tup4 string (tup2 ID.db string) string (tup2 ptime ptime))
        (sprintf "INSERT INTO %s (stream,id,type,input,date_start,date_end) \
                  VALUES (?,?,?,?,?,?)" table)
    in Conn.request db Request.(
      with_trans (List.fold_left (fun acc s ->
                      acc >>= fun () -> exec insert s)
                    (return ()) data))

  let bump_streams db streams =
    let table  = (Conn.names db).streams in
    let now    = Time.Clock.now_s () in
    let data   = List.map (fun (s:Stream.t) -> ID.to_db s.id, now) streams in
    let update_last =
      R.exec Types.(tup2 ID.db ptime)
        (sprintf {|UPDATE %s SET date_end = $2
                  WHERE id = $1 AND date_start = (SELECT date_start FROM %s
                  WHERE id = $1 ORDER BY date_start DESC LIMIT 1)|}
           table table)
    in
    Conn.request db Request.(
      with_trans (List.fold_left (fun acc s ->
                      acc >>= fun () -> exec update_last s)
                    (return ()) data))

  let select_stream_unique  db ?(inputs = []) ?(ids = []) ~from ~till () =
    let table  = (Conn.names db).streams in
    let ids = is_in "id" ID.to_value_string ids in
    let inputs =
      is_in "input" (fun i ->
          sprintf "'%s'"
          @@ Yojson.Safe.to_string
          @@ Topology.topo_input_to_yojson i) inputs in
    let select =
      R.collect Types.(tup2 ptime ptime) Types.(tup4 string ID.db string ptime)
        (sprintf {|SELECT DISTINCT ON (type, id) stream,id,type,date_end FROM %s
                  WHERE %s %s date_end >= $1 AND date_start <= $2
                  ORDER BY type, id, date_end DESC|} table ids inputs) in
    Conn.request db Request.(
      list select (from,till) >>= fun data ->
      try
        let data =
          List.map (fun (s, id, typ, t) ->
              Result.get_exn @@ Stream.of_yojson @@ Yojson.Safe.from_string s,
              ID.of_db id,
              Stream.typ_of_string typ,
              t) data in
        return (Ok (Compressed { data }))
      with _ -> return (Error "Stream parser failure"))

  let select_stream_ids  db ~from ~till () =
    let table  = (Conn.names db).streams in
    let select = R.collect Types.(tup2 ptime ptime) Types.(tup2 ID.db ptime)
                   (sprintf {|SELECT id,MAX(date_end) FROM %s
                             WHERE date_end >= $1 AND date_start <= $2
                             GROUP BY id|} table) in
    Conn.request db Request.(
      list select (from, till)
      >>= fun data ->
      let data = List.map (fun (id, time) -> ID.of_db id, time) data in
      return (Compressed { data }))

  let select_streams ?(limit = 500) ?(ids = []) ?(inputs = [])
        ~from ~till db =
    let table  = (Conn.names db).streams in
    let ids    = is_in "id" ID.to_value_string ids in
    let inputs =
      is_in "input" (fun i ->
          sprintf "'%s'::JSONB"
          @@ Yojson.Safe.to_string
          @@ Topology.topo_input_to_yojson i) inputs in
    let select =
      R.collect Types.(tup3 ptime ptime int) Types.(tup3 string ptime ptime)
        (sprintf {|SELECT stream,date_start,date_end FROM %s
                  WHERE %s %s date_start >= $1 AND date_end <= $2
                  ORDER BY date_end DESC LIMIT $3|} table ids inputs) in
    Conn.request db Request.(
      list select (from, till, limit) >>= fun l ->
      try let data =
            List.map (fun (s,f,t) ->
                Result.get_exn
                @@ Common.Stream.of_yojson
                @@ Yojson.Safe.from_string s,
                f,t) l
          in return @@ Ok (Raw { data
                               ; has_more = List.length data >= limit
                               ; order = `Desc })
      with _ -> return @@ Error "select_streams failure")

  let insert' db table data to_time to_yojson =
    let data  =
      List.map (fun ((stream:Stream.t), x) ->
          ID.to_db stream.id,
          Yojson.Safe.to_string @@ to_yojson x,
          to_time x) data in
    let insert =
      R.exec Types.(tup3 ID.db string ptime)
        (sprintf "INSERT INTO %s (stream,data,date) VALUES (?,?,?)" table)
    in Conn.request db Request.(
      with_trans (List.fold_left (fun acc v ->
                      acc >>= fun () -> exec insert v) (return ()) data))

  let insert_ts_info db info =
    let open Streams.TS in
    let table = (Conn.names db).ts_info in
    insert' db table info (fun (x:info) -> x.timestamp)
      info_to_yojson

  let insert_services db services =
    let open Streams.TS in
    let table = (Conn.names db).services in
    insert' db table services (fun (x:services) -> x.timestamp)
      services_to_yojson

  let insert_tables db tables =
    let open Streams.TS in
    let table = (Conn.names db).tables in
    insert' db table tables (fun (x:tables) -> x.timestamp)
      tables_to_yojson

  let insert_pids db pids =
    let open Streams.TS in
    let table = (Conn.names db).pids in
    insert' db table pids (fun (x:pids) -> x.timestamp)
      pids_to_yojson

  let insert_bitrate db bitrate =
    let open Streams.TS in
    let table = (Conn.names db).bitrate in
    insert' db table bitrate (fun (x:bitrate) -> x.timestamp)
      bitrate_to_yojson

  let insert_t2mi_info db info =
    let open Streams.T2MI in
    let table = (Conn.names db).t2mi_info in
    insert' db table info (fun (x:structure) -> x.timestamp)
      structure_to_yojson

  let select' ?(with_pre = true) ?(limit = 500)
        ?(ids  = []) ~from ~till db table of_yojson =
    let ids    = is_in "stream" ID.to_value_string ids in
    let select =
      R.collect Types.(tup3 ptime ptime int) Types.(tup3 ID.db string ptime)
        (sprintf {|SELECT * FROM %s WHERE %s date >= $1 AND date <= $2
                  ORDER BY date DESC LIMIT $3|} table ids) in
    let select_pre =
      R.collect Types.(tup3 ptime ptime int) Types.(tup3 ID.db string ptime)
        (sprintf {|(SELECT * FROM %s WHERE %s date >= $1 AND date <= $2
                  ORDER BY date DESC)
                  UNION ALL
                  (SELECT * FROM %s WHERE %s date < $1
                  ORDER BY date DESC LIMIT 1)
                  ORDER BY date LIMIT $3|} table ids table ids) in
    Conn.request db Request.(
      list (if with_pre then select_pre else select) (from, till, limit)
      >>= fun l ->
      try let data =
            List.map (fun (id, s, _) ->
                ID.of_db id,
                unwrap @@ of_yojson @@ Yojson.Safe.from_string s) l
          in return @@ Ok (Raw { data
                               ; has_more = List.length data >= limit
                               ; order    = `Desc })
      with e -> return @@ Error (Printexc.to_string e))

  let select_ts_info ?with_pre ?limit ?ids ~from ~till db =
    let open Streams.TS in
    let table = (Conn.names db).ts_info in
    select' ?with_pre ?limit ?ids ~from ~till db table info_of_yojson

  let select_services ?with_pre ?limit ?ids ~from ~till db =
    let open Streams.TS in
    let table = (Conn.names db).services in
    select' ?with_pre ?limit ?ids ~from ~till db table services_of_yojson

  let select_tables ?with_pre ?limit ?ids ~from ~till db =
    let open Streams.TS in
    let table = (Conn.names db).tables in
    select' ?with_pre ?limit ?ids ~from ~till db table tables_of_yojson

  let select_pids ?with_pre ?limit ?ids ~from ~till db =
    let open Streams.TS in
    let table = (Conn.names db).pids in
    select' ?with_pre ?limit ?ids ~from ~till db table pids_of_yojson

  let select_t2mi_info ?with_pre ?limit ?ids ~from ~till db =
    let open Streams.T2MI in
    let table = (Conn.names db).t2mi_info in
    select' ?with_pre ?limit ?ids ~from ~till db table structure_of_yojson

end

module Errors = struct
  open Board_types.Errors

  let error = Types.(
      custom List.(ID.db & int & int & int & int & bool
                   & int & int32 & int32 & int32 & ptime)
        ~encode:(fun (id, (err : Errors.t)) ->
          Ok (ID.to_db id,
              (err.count,
               (err.err_code,
                (err.err_ext,
                 (err.priority,
                  (err.multi_pid,
                   (err.pid,
                    (err.packet,
                     (err.param_1,(err.param_2, err.timestamp)))))))))))
        ~decode:(fun (id,
                      (count,
                       (err_code,
                        (err_ext,
                         (priority,
                          (multi_pid,
                           (pid,
                            (packet,
                             (param_1, (param_2, timestamp)))))))))) ->
          Ok (ID.of_db id,
              { timestamp
              ; count
              ; err_code
              ; err_ext
              ; priority
              ; multi_pid
              ; pid
              ; service = None (* FIXME *)
              ; packet
              ; param_1
              ; param_2
              })))

  let insert db ~is_ts errs =
    let open Printf in
    let table = (Conn.names db).errors in
    let errs =
      List.map (fun ((s:Stream.t), e) -> List.map (Pair.make s.id) e) errs
      |> List.concat in
    let insert =
      R.exec Types.(tup2 bool error)
        (sprintf {|INSERT INTO %s(is_ts,stream,count,err_code,err_ext,priority,
                  multi_pid,pid,packet,param_1,param_2,date)
                  VALUES (?,?,?,?,?,?,?,?,?,?,?,?)|} table)
    in Conn.request db Request.(
      with_trans (List.fold_left (fun acc x ->
                      acc >>= fun () -> exec insert (is_ts,x))
                    (return ()) errs))

  let select_has_any db ?(streams = [])
        ?(priority = []) ?(pids = [])
        ?(errors = []) ~is_ts ~from ~till () =
    let open Printf in
    let table = (Conn.names db).errors in
    let streams  = is_in "stream" Stream.ID.to_string streams in
    let priority = is_in "priority" string_of_int priority in
    let pids     = is_in "pid" string_of_int pids in
    let errors   = is_in "err_code"  string_of_int errors in
    let select =
      R.find Types.(tup3 bool ptime ptime) Types.bool
        (sprintf {|SELECT EXISTS (SELECT 1 FROM %s
                  WHERE %s %s %s %s is_ts = ? AND date >= ? AND date <= ?)|}
           table streams priority pids errors)
    in Conn.request db Request.(
      find select (is_ts,from,till)
      >>= function None   -> return false
                 | Some x -> return x)

  let select_percent db ?(streams = [])
        ?(priority = []) ?(pids = [])
        ?(errors = []) ~is_ts ~from ~till () =
    let open Printf in
    let table = (Conn.names db).errors in
    let streams  = is_in "stream" ID.to_value_string streams in
    let priority = is_in "priority" string_of_int priority in
    let pids     = is_in "pid" string_of_int pids in
    let errors   = is_in "err_code"  string_of_int errors in
    let span = Time.(Period.to_float_s @@ diff till from) in
    let select =
      R.find Types.(tup3 bool ptime ptime) Types.int
        (sprintf {|SELECT count(DISTINCT date_trunc('second',date)) FROM %s 
                  WHERE %s %s %s %s is_ts = ? AND date >= ? AND date <= ?|}
           table streams priority pids errors)
    in Conn.request db Request.(
      find select (is_ts,from,till) >>= function
      | None   -> return 0.0
      | Some s -> return (100. *. (float_of_int s) /. span))

  let select_errors db ?(streams = [])
        ?(priority = []) ?(pids = [])
        ?(errors = []) ?(limit = 500)
        ~is_ts ~from ~till () =
    let open Printf in
    let table    = (Conn.names db).errors in
    let streams  = is_in "stream" ID.to_value_string streams in
    let priority = is_in "priority" string_of_int priority in
    let pids     = is_in "pid" string_of_int pids in
    let errors   = is_in "err_code"  string_of_int errors in
    let select =
      R.collect Types.(tup4 bool ptime ptime int) error
        (sprintf {|SELECT stream,count,err_code,err_ext,priority,
                  multi_pid,pid,packet,param_1,param_2,date FROM %s 
                  WHERE %s %s %s %s is_ts = ? AND date >= ? AND date <= ?
                  ORDER BY date DESC LIMIT ?|}
           table streams priority pids errors)
    in Conn.request db Request.(
      list select (is_ts, from, till, limit) >>= fun data ->
      return (Raw { data
                  ; has_more = List.length data >= limit
                  ; order    = `Desc }))

  (* TODO consider stream presence *)
  let select_errors_compressed db ?(streams = [])
        ?(priority = []) ?(pids = [])
        ?(errors = []) ~is_ts ~from ~till () =
    let open Printf in
    let table    = (Conn.names db).errors in
    let streams  = is_in "stream" ID.to_value_string streams in
    let priority = is_in "priority" string_of_int priority in
    let pids     = is_in "pid" string_of_int pids in
    let errors   = is_in "err_code"  string_of_int errors in
    let intvals = Time.split ~from ~till in
    let select =
      R.find Types.(tup3 bool ptime ptime ) Types.int
        (sprintf {|SELECT count(DISTINCT date_trunc('second',date)) FROM %s
                  WHERE %s %s %s %s is_ts = ? AND date >= ? AND date <= ?|}
           table streams priority pids errors)
    in Conn.request db Request.(
      with_trans (
          List.fold_left (fun acc (f, t) ->
              let span = Time.(Period.to_float_s @@ diff till from) in
              acc >>= fun l ->
              find select (is_ts, f, t) >>= function
              | None -> return l
              | Some s -> let perc = (100. *. (float_of_int s) /. span) in
                          return ((perc,f,t)::l))
            (return []) intvals))
       >>= fun l ->
       List.fold_left (fun acc (p,from,till) ->
           acc >>= fun acc ->
           Device.select_state_compressed_internal db ~from ~till
           >>= fun (f,_,_) ->
           Lwt.return ( { errors    = 100. *. p /. f
                        ; no_stream = 100. -. f
                        ; period    = from, till } :: acc))
         (Lwt.return []) l
       >|= fun data -> Compressed { data }

end
