open Storage.Database
open Board_qos_types
open Db_common
open Printf
open Containers
open Common

let typ : (ID.t * Pid.t Time.timespan) Caqti_type.t =
  Types.custom
    Types.(List.(ID.db & int & option int & string
                 & bool & bool & bool & bool
                 & ptime & ptime))
    ~encode:(fun (id, ({ from; till; data = (pid, data) } : Pid.t Time.timespan)) ->
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
               ; service_name = None
               ; typ
               } in
             (ID.of_db id, { from; till; data = (pid, data) })))

let insert db (pids : (ID.t * Pid.t Time.timespan list) list) =
  let table = (Conn.names db).pids in
  let pids =
    List.map (fun (id, pids) -> List.map (Pair.make id) pids) pids
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

let bump db (pids : (ID.t * Pid.t Time.timespan list) list) =
  let table = (Conn.names db).pids in
  let data =
    List.map (fun (id, pids) ->
        List.map (fun ({ data = (pid, _); till; _ } : Pid.t Time.timespan) ->
            ID.to_db id, pid, till) pids) pids
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
