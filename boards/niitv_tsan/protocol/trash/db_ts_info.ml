open Storage.Database
open Db_common
open Printf
open Containers
open Board_types.Ts_info
open Common
open Api.Api_types

let typ : (ID.t * t Time.timespan) Caqti_type.t =
  Types.custom
    Types.(
      List.(
        ID.db
        & bool
        & int
        & int
        & int
        & int
        & int
        & string
        & string
        & ptime
        & ptime))
    ~encode:(fun (id, ({ from; till; data } : t Time.timespan)) ->
      Ok
        ( ID.to_db id,
          ( data.complete,
            ( data.services_num,
              ( data.nw_pid,
                ( data.ts_id,
                  ( data.nw_id,
                    ( data.orig_nw_id,
                      (data.nw_name, (data.bouquet_name, (from, till))) ) ) ) )
            ) ) ))
    ~decode:
      (fun ( id,
             ( complete,
               ( services_num,
                 ( nw_pid,
                   ( ts_id,
                     ( nw_id,
                       (orig_nw_id, (nw_name, (bouquet_name, (from, till)))) )
                   ) ) ) ) ) ->
      Ok
        (let (data : t) =
           {
             complete;
             services_num;
             nw_pid;
             ts_id;
             nw_id;
             orig_nw_id;
             nw_name;
             bouquet_name;
           }
         in
         (ID.of_db id, { from; till; data })))

let insert db (info : (Stream.ID.t * t Time.timespan) list) =
  let table = (Conn.names db).ts_info in
  let insert =
    R.exec typ
      (sprintf
         {|INSERT INTO %s(stream,complete,services,nw_pid,ts_id,nw_id,
                orig_nw_id,nw_name,bouquet_name,date_start,date_end)
                VALUES (?,?,?,?,?,?,?,?,?,?,?)
                ON CONFLICT DO NOTHING|}
         table)
  in
  Conn.request db
    Request.(
      with_trans
        (List.fold_left
           (fun acc ((id : Stream.ID.t), info) ->
             acc >>= fun () -> exec insert (id, info))
           (return ()) info))

let bump db (info : (Stream.ID.t * t Time.timespan) list) =
  let table = (Conn.names db).ts_info in
  let data =
    List.map
      (fun (id, ({ till; _ } : t Time.timespan)) -> (ID.to_db id, till))
      info
  in
  let update_last =
    R.exec
      Types.(tup2 ID.db ptime)
      (sprintf
         {|UPDATE %s SET date_end = $3
                WHERE stream = $1
                AND date_start = (SELECT date_start FROM %s
                WHERE stream = $1
                ORDER BY date_start DESC LIMIT 1)|}
         table table)
  in
  Conn.request db
    Request.(
      with_trans
        (List.fold_left
           (fun acc s -> acc >>= fun () -> exec update_last s)
           (return ()) data))

let select ?(with_pre = true) ?(limit = 500) ?(ids = []) ~from ~till db =
  let table = (Conn.names db).ts_info in
  let ids = is_in "stream" ID.to_value_string ids in
  let select =
    R.collect
      Types.(tup3 ptime ptime int)
      typ
      (sprintf
         {|SELECT * FROM %s
                WHERE %s date >= $1 AND date <= $2
                ORDER BY date DESC LIMIT $3|}
         table ids)
  in
  let select_pre =
    R.collect
      Types.(tup3 ptime ptime int)
      typ
      (sprintf
         {|(SELECT * FROM %s
                WHERE %s date >= $1 AND date <= $2
                ORDER BY date DESC)
                UNION ALL
                (SELECT * FROM %s WHERE %s date < $1
                ORDER BY date DESC LIMIT 1)
                ORDER BY date LIMIT $3|}
         table ids table ids)
  in
  Conn.request db
    Request.(
      list (if with_pre then select_pre else select) (from, till, limit)
      >>= fun data ->
      try
        return
        @@ Ok
             (Raw { data; has_more = List.length data >= limit; order = `Desc })
      with e -> return @@ Error (Printexc.to_string e))
