open Containers
open Storage.Database
open Api.Api_types
open Board_qos_types
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
    make_keys ~time_key:"date_end"
      [ "state",      key "INTEGER"
      ; "date_start", key "TIMESTAMP"
      ; "date_end",   key "TIMESTAMP"
      ]

  let keys_streams =
    make_keys ~time_key:"date_end"
      [ "stream", key "JSONB"
      ; "id", key ID.typ
      ; "incoming", key "BOOL"
      ; "type", key "TEXT"
      ; "input", key "JSONB"
      ; "date_start", key "TIMESTAMP"
      ; "date_end", key "TIMESTAMP"
      ]

  let keys_ts_info =
    make_keys ~time_key:"date"
      [ "stream", key ID.typ
      ; "complete", key "BOOL"
      ; "services", key "INTEGER"
      ; "nw_pid", key "INTEGER"
      ; "ts_id", key "INTEGER"
      ; "nw_id", key "INTEGER"
      ; "orig_nw_id", key "INTEGER"
      ; "nw_name", key "TEXT"
      ; "bouquet_name", key "TEXT"
      ; "date", key "TIMESTAMP"
      ]

  let keys_services =
    make_keys ~time_key:"date_end"
      [ "stream", key ~primary:true ID.typ
      ; "id", key ~primary:true "INTEGER"
      ; "name", key "TEXT"
      ; "provider", key "TEXT"
      ; "pmt_pid", key "INTEGER"
      ; "pcr_pid", key "INTEGER"
      ; "has_pmt", key "BOOL"
      ; "has_sdt", key "BOOL"
      ; "dscr", key "BOOL"
      ; "dscr_list", key "BOOL"
      ; "eit_schedule", key "BOOL"
      ; "eit_pf", key "BOOL"
      ; "free_ca_mode", key "BOOL"
      ; "running_status", key "INTEGER"
      ; "service_type", key "INTEGER"
      ; "service_type_list", key "INTEGER"
      ; "elements", key "JSONB"
      ; "date_start", key ~primary:true "TIMESTAMP"
      ; "date_end", key "TIMESTAMP"
      ]

  let keys_tables =
    make_keys ~time_key:"date"
      [ "stream", key ID.typ
      ; "data", key "TEXT"
      ; "date", key "TIMESTAMP"
      ]

  let keys_pids =
    make_keys ~time_key:"date_end"
      [ "stream", key ~primary:true ID.typ
      ; "pid", key ~primary:true "INTEGER"
      ; "service", key "TEXT"
      ; "type", key "JSONB"
      ; "has_pts", key "BOOL"
      ; "has_pcr", key "BOOL"
      ; "scrambled", key "BOOL"
      ; "present", key "BOOL"
      ; "date_start", key ~primary:true "TIMESTAMP"
      ; "date_end", key "TIMESTAMP"
      ]

  let keys_t2mi_info =
    make_keys ~time_key:"date"
      [ "stream", key ID.typ
      ; "data", key "TEXT"
      ; "date", key "TIMESTAMP"
      ]

  let keys_bitrate =
    make_keys ~time_key:"date"
      [ "stream", key ID.typ
      ; "data", key "TEXT"
      ; "date", key "TIMESTAMP"
      ]

  let keys_errors =
    make_keys ~time_key:"date"
      [ "is_ts",     key "BOOL"
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
  | lst -> Printf.sprintf " %s IN (%s) AND "
             field (String.concat "," @@ List.map to_string lst)

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
    let select =
      R.collect Types.(tup3 ptime ptime int) Types.(tup3 int ptime ptime)
        (sprintf {|SELECT * FROM %s WHERE date_start <= $2 AND date_end >= $1 
                  ORDER BY date_end DESC LIMIT $3|} table)
    in Conn.request db Request.(
      list select (from,till,limit) >>= fun l ->
      let data =
        List.map (fun (st, from, till) ->
            { state = state_of_int st; from; till }) l in
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
    Conn.request db Request.(
      find select_o (from, till) >>= function
      | Some s ->
         let fine, init, no_response = match state_of_int s with
           | `Fine -> (100.,0.,0.)
           | `Init -> (0.,100.,0.)
           | `No_response -> (0.,0.,100.) in
         return { fine; init; no_response }
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
         let (fine, init, no_response) =
           List.fold_left (fun (f, i, n) -> function
               | `Fine, x -> (f +. x, i, n)
               | `Init, x -> (f, i +. x, n)
               | `No_response, x -> (f, i, n +. x)) (0., 0., 0.) i
         in return { fine; init; no_response })

  let select_state_compressed db ~from ~till =
    select_state_compressed_internal db ~from ~till
    >|= fun data -> Compressed { data }
    
end

module Streams = struct

  type state = Common.Topology.state

  let insert_streams db streams =
    let table = (Conn.names db).streams in
    let now = Time.Clock.now_s () in
    let data =
      List.map (fun (incoming, s) ->
          (Yojson.Safe.to_string @@ Stream.to_yojson s, ID.to_db s.id),
          (incoming, Stream.typ_to_string s.typ),
          Yojson.Safe.to_string
          @@ Topology.topo_input_to_yojson
          @@ Option.get_exn @@ Stream.get_input s, (* FIXME make optional in table *)
          (now, now)) streams in
    let insert =
      R.exec Types.(tup4 (tup2 string ID.db) (tup2 bool string) string (tup2 ptime ptime))
        (sprintf "INSERT INTO %s (stream,id,incoming,type,input,date_start,date_end) \
                  VALUES (?,?,?,?,?,?,?)" table)
    in Conn.request db Request.(
      with_trans (List.fold_left (fun acc s ->
                      acc >>= fun () -> exec insert s)
                    (return ()) data))

  let bump_streams db streams =
    let table = (Conn.names db).streams in
    let now = Time.Clock.now_s () in
    let data = List.map (fun (s:Stream.t) -> ID.to_db s.id, now) streams in
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

  let select_stream_unique db
        ?(inputs = []) ?(ids = []) ?incoming
        ~from ~till () =
    let table = (Conn.names db).streams in
    let ids = is_in "id" ID.to_value_string ids in
    let incoming = match incoming with
      | Some true -> "incoming = true AND"
      | _ -> "" in
    let inputs =
      is_in "input" (fun i ->
          sprintf "'%s'"
          @@ Yojson.Safe.to_string
          @@ Topology.topo_input_to_yojson i) inputs in
    let select =
      R.collect Types.(tup2 ptime ptime) Types.(tup3 string ID.db ptime)
        (sprintf {|SELECT DISTINCT ON (id) stream,id,date_end FROM %s
                  WHERE %s %s %s date_end >= $1 AND date_start <= $2
                  ORDER BY id, date_end DESC|} table ids inputs incoming) in
    Conn.request db Request.(
      list select (from, till)
      >>= fun data ->
      try
        let data =
          List.map (fun (s, id, t) ->
              Result.get_exn @@ Stream.of_yojson @@ Yojson.Safe.from_string s,
              ID.of_db id,
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

  let select_streams ?(limit = 500) ?(ids = []) ?(inputs = []) ?incoming
        ~from ~till db =
    let table = (Conn.names db).streams in
    let ids = is_in "id" ID.to_value_string ids in
    let incoming = match incoming with
      | Some true -> "incoming = true AND"
      | _ -> "" in
    let inputs =
      is_in "input" (fun i ->
          sprintf "'%s'::JSONB"
          @@ Yojson.Safe.to_string
          @@ Topology.topo_input_to_yojson i) inputs in
    let select =
      R.collect Types.(tup3 ptime ptime int) Types.(tup3 string ptime ptime)
        (sprintf {|SELECT stream,date_start,date_end FROM %s
                  WHERE %s %s %s date_start >= $1 AND date_end <= $2
                  ORDER BY date_end DESC LIMIT $3|} table ids inputs incoming) in
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

  let select' ?(with_pre = true) ?(limit = 500)
        ?(ids  = []) ~from ~till db table of_data =
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
            List.map (fun (id, s, date) ->
                of_data (ID.of_db id,
                         Yojson.Safe.from_string s,
                         date)) l
          in return @@ Ok (Raw { data
                               ; has_more = List.length data >= limit
                               ; order    = `Desc })
      with e -> return @@ Error (Printexc.to_string e))

end

module Ts_info = struct

  open Board_types.Ts_info

  let typ : (ID.t * t timestamped) Caqti_type.t =
    Types.custom
      Types.(List.(ID.db & bool & int & int & int & int
                   & int & string & string & ptime))
      ~encode:(fun (id, ({ timestamp; data } : t timestamped)) ->
        Ok (ID.to_db id,
            (data.complete,
             (data.services_num,
              (data.nw_pid,
               (data.ts_id,
                (data.nw_id,
                 (data.orig_nw_id,
                  (data.nw_name,
                   (data.bouquet_name, timestamp))))))))))
      ~decode:(fun (id,
                    (complete,
                     (services_num,
                      (nw_pid,
                       (ts_id,
                        (nw_id,
                         (orig_nw_id,
                          (nw_name,
                           (bouquet_name, timestamp))))))))) ->
        Ok (let (data : t) =
              { complete
              ; services_num
              ; nw_pid
              ; ts_id
              ; nw_id
              ; orig_nw_id
              ; nw_name
              ; bouquet_name
              } in
            ID.of_db id, { timestamp; data }))

  let insert db (info : (Stream.ID.t * t timestamped) list) =
    let table = (Conn.names db).ts_info in
    let insert =
      R.exec typ
        (sprintf {|INSERT INTO %s(stream,complete,services,nw_pid,ts_id,nw_id,
                  orig_nw_id,nw_name,bouquet_name,date)
                  VALUES (?,?,?,?,?,?,?,?,?,?)|} table)
    in Conn.request db Request.(
      with_trans (List.fold_left (fun acc ((id : Stream.ID.t), info) ->
                      acc >>= fun () -> exec insert (id, info))
                    (return ()) info))

  let select ?(with_pre = true) ?(limit = 500)
        ?(ids = []) ~from ~till db =
    let table = (Conn.names db).ts_info in
    let ids = is_in "stream" ID.to_value_string ids in
    let select =
      R.collect Types.(tup3 ptime ptime int) typ
        (sprintf {|SELECT * FROM %s
                  WHERE %s date >= $1 AND date <= $2
                  ORDER BY date DESC LIMIT $3|} table ids) in
    let select_pre =
      R.collect Types.(tup3 ptime ptime int) typ
        (sprintf {|(SELECT * FROM %s
                  WHERE %s date >= $1 AND date <= $2
                  ORDER BY date DESC)
                  UNION ALL
                  (SELECT * FROM %s WHERE %s date < $1
                  ORDER BY date DESC LIMIT 1)
                  ORDER BY date LIMIT $3|} table ids table ids) in
    Conn.request db Request.(
      list (if with_pre then select_pre else select) (from, till, limit)
      >>= fun data ->
      try return @@ Ok (Raw { data
                            ; has_more = List.length data >= limit
                            ; order = `Desc })
      with e -> return @@ Error (Printexc.to_string e))

end

module Pids = struct

  let typ : (ID.t * Pid.t timespan) Caqti_type.t =
    Types.custom
      Types.(List.(ID.db & int & option int & string
                   & bool & bool & bool & bool
                   & ptime & ptime))
      ~encode:(fun (id, ({ from; till; data = (pid, data) } : Pid.t timespan)) ->
        let typ = Pid.typ_to_yojson data.typ |> Yojson.Safe.to_string in
        Ok (ID.to_db id,
            (pid,
             (data.service_id,
              (typ,
               (data.has_pts,
                (data.has_pcr,
                 (data.scrambled,
                  (data.present,
                   (from, till))))))))))
      ~decode:(fun (id,
                    (pid,
                     (service_id,
                      (typ,
                       (has_pts,
                        (has_pcr,
                         (scrambled,
                          (present,
                           (from, till))))))))) ->
        match Pid.typ_of_yojson @@ Yojson.Safe.from_string typ with
        | Error e -> Error e
        | Ok typ ->
           Ok (let (data : Pid.info) =
                 { has_pts
                 ; has_pcr
                 ; scrambled
                 ; present
                 ; service_id
                 ; typ
                 } in
               (ID.of_db id, { from; till; data = (pid, data) })))

  let insert db (pids : (ID.t * Pid.t timestamped list) list) =
    let table = (Conn.names db).pids in
    let pids =
      List.map (fun (id, pids) ->
          List.map (fun { timestamp; data } ->
              let from, till = timestamp, timestamp in
              id, { from; till; data }) pids) pids
      |> List.concat in
    let insert =
      R.exec typ
        (sprintf {|INSERT INTO %s (stream,pid,service,type,
                  has_pts,has_pcr,scrambled,present,
                  date_start,date_end)
                  VALUES (?,?,?,?,?,?,?,?,?,?)
                  ON CONFLICT DO NOTHING|} table)
    in Conn.request db Request.(
      with_trans (List.fold_left (fun acc v ->
                      acc >>= fun () -> exec insert v) (return ()) pids))
  
  let bump ?(now = false) db (pids : (ID.t * Pid.t timestamped list) list) =
    let table = (Conn.names db).pids in
    let data =
      List.map (fun (id, pids) ->
          List.map (fun ({ data = (pid, _); timestamp } : Pid.t timestamped) ->
              let timestamp = match now with
                | true -> Time.Clock.now_s ()
                | false -> timestamp in
              ID.to_db id, pid, timestamp) pids) pids
      |> List.concat in
    let update_last =
      R.exec Types.(tup3 ID.db int ptime)
        (sprintf {|UPDATE %s SET date_end = $3
                  WHERE stream = $1
                  AND pid = $2
                  AND date_start = (SELECT date_start FROM %s
                  WHERE stream = $1 AND pid = $2
                  ORDER BY date_start DESC LIMIT 1)|}
           table table)
    in
    Conn.request db Request.(
      with_trans (List.fold_left (fun acc s ->
                      acc >>= fun () -> exec update_last s)
                    (return ()) data))

end

module Errors = struct

  open Board_types.Error

  let error =
    Types.custom
      Types.(List.(ID.db & int & int & int & int & bool
                   & int & int32 & int32 & int32 & ptime & option string))
      ~encode:(fun (id, (err : Error.t)) ->
        Ok (ID.to_db id,
            (err.count,
             (err.err_code,
              (err.err_ext,
               (err.priority,
                (err.multi_pid,
                 (err.pid,
                  (err.packet,
                   (err.param_1,
                    (err.param_2,
                     (err.timestamp, err.service))))))))))))
      ~decode:(fun (id,
                    (count,
                     (err_code,
                      (err_ext,
                       (priority,
                        (multi_pid,
                         (pid,
                          (packet,
                           (param_1,
                            (param_2,
                             (timestamp, service))))))))))) ->
        let error =
          { timestamp
          ; count
          ; err_code
          ; err_ext
          ; priority
          ; multi_pid
          ; pid
          ; service
          ; packet
          ; param_1
          ; param_2
          } in
        Ok (ID.of_db id, error))

  let insert db ~is_ts errs =
    let open Printf in
    let table = (Conn.names db).errors in
    let errs =
      List.map (fun ((id : Stream.ID.t), e) -> List.map (Pair.make id) e) errs
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
    let streams  = is_in "stream" ID.to_value_string streams in
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
        ?(errors = []) ?(limit = 500) ?(order = `Desc)
        ~is_ts ~from ~till () =
    let open Printf in
    let table = (Conn.names db).errors in
    let pids_table = (Conn.names db).pids in
    let streams = is_in "stream" ID.to_value_string streams in
    let priority = is_in "priority" string_of_int priority in
    let pids = is_in "pid" string_of_int pids in
    let errors = is_in "err_code" string_of_int errors in
    let ord = match order with `Desc -> "DESC" | `Asc -> "ASC" in
    let select =
      R.collect Types.(tup4 bool ptime ptime int) error
        (sprintf {|SELECT e.stream,e.count,e.err_code,e.err_ext,e.priority,
                  e.multi_pid,e.pid,e.packet,e.param_1,e.param_2,e.date,p.service
                  FROM %s e
                  LEFT JOIN LATERAL (
                      SELECT obj.value->>'service' AS service
                      FROM %s pids
                      JOIN LATERAL jsonb_array_elements(pids.data) obj(value)
                      ON (obj.value->>'pid')::INT = e.pid::INT
                      WHERE pids.date <= e.date
                      AND pids.stream = e.stream
                      ORDER BY date DESC LIMIT 1) p
                  ON TRUE
                  WHERE %s %s %s %s is_ts = ? AND date >= ? AND date <= ?
                  ORDER BY date %s
                  LIMIT ?|}
           table pids_table streams priority pids errors ord)
    in Conn.request db Request.(
      list select (is_ts, from, till, limit)
      >>= fun data ->
      return (Raw { data
                  ; has_more = List.length data >= limit
                  ; order }))

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
           >>= fun { fine; _ } ->
           Lwt.return ( { errors    = 100. *. p /. fine
                        ; no_stream = 100. -. fine
                        ; period    = from, till } :: acc))
         (Lwt.return []) l
       >|= fun data -> Compressed { data }

end
