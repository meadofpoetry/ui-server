open Containers
open Storage.Database
open Api.Api_types
open Board_qos_types
open Common
open Printf

include Db_common

module Device = Db_device
module Services = Db_services
module Pids = Db_pids
module Errors = Db_errors

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
                { data = Result.get_exn
                         @@ Common.Stream.of_yojson
                         @@ Yojson.Safe.from_string s
                ; from = f
                ; till = t }) l
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

  let typ : (ID.t * t timespan) Caqti_type.t =
    Types.custom
      Types.(List.(ID.db & bool & int & int & int & int
                   & int & string & string & ptime & ptime))
      ~encode:(fun (id, ({ from; till; data } : t timespan)) ->
        Ok (ID.to_db id,
            (data.complete,
             (data.services_num,
              (data.nw_pid,
               (data.ts_id,
                (data.nw_id,
                 (data.orig_nw_id,
                  (data.nw_name,
                   (data.bouquet_name,
                    (from, till)))))))))))
      ~decode:(fun (id,
                    (complete,
                     (services_num,
                      (nw_pid,
                       (ts_id,
                        (nw_id,
                         (orig_nw_id,
                          (nw_name,
                           (bouquet_name,
                            (from, till)))))))))) ->
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
            ID.of_db id, { from; till; data }))

  let insert db (info : (Stream.ID.t * t timespan) list) =
    let table = (Conn.names db).ts_info in
    let insert =
      R.exec typ
        (sprintf {|INSERT INTO %s(stream,complete,services,nw_pid,ts_id,nw_id,
                  orig_nw_id,nw_name,bouquet_name,date_start,date_end)
                  VALUES (?,?,?,?,?,?,?,?,?,?,?)
                  ON CONFLICT DO NOTHING|} table)
    in
    Conn.request db Request.(
      with_trans (List.fold_left (fun acc ((id : Stream.ID.t), info) ->
                      acc >>= fun () -> exec insert (id, info))
                    (return ()) info))

  let bump db (info : (Stream.ID.t * t timespan) list) =
    let table = (Conn.names db).ts_info in
    let data =
      List.map (fun (id, ({ till; _ } : t timespan)) ->
          ID.to_db id, till) info in
    let update_last =
      R.exec Types.(tup2 ID.db ptime)
        (sprintf {|UPDATE %s SET date_end = $3
                  WHERE stream = $1
                  AND date_start = (SELECT date_start FROM %s
                  WHERE stream = $1
                  ORDER BY date_start DESC LIMIT 1)|}
           table table)
    in
    Conn.request db Request.(
      with_trans (List.fold_left (fun acc s ->
                      acc >>= fun () -> exec update_last s)
                    (return ()) data))

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

module Bitrate = struct

  open Bitrate

  let insert db (data : bitrates) =
    let table = (Conn.names db).bitrate in
    let data =
      List.map (fun (id, ({ data; timestamp } : t timestamped)) ->
          ID.to_db id, data.total, timestamp) data in
    let insert =
      R.exec Types.(tup3 ID.db int ptime)
        (sprintf {|INSERT INTO %s (stream,bitrate,date)
                  VALUES (?,?,?)
                  ON CONFLICT DO NOTHING|} table)
    in
    Conn.request db Request.(
      with_trans (List.fold_left (fun acc v ->
                      acc >>= fun () -> exec insert v) (return ()) data))

  let insert_pids db (data : bitrates) =
    let table = (Conn.names db).pids_bitrate in
    let data =
      List.map (fun (id, ({ data; timestamp } : t timestamped)) ->
          List.map (fun (pid, rate) ->
              ID.to_db id, pid, rate, timestamp) data.pids) data
      |> List.concat in
    let insert =
      R.exec Types.(tup4 ID.db int int ptime)
        (sprintf {|INSERT INTO %s (stream,pid,bitrate,date)
                  VALUES (?,?,?,?)
                  ON CONFLICT DO NOTHING|} table)
    in
    Conn.request db Request.(
      with_trans (List.fold_left (fun acc v ->
                      acc >>= fun () -> exec insert v) (return ()) data))

end
