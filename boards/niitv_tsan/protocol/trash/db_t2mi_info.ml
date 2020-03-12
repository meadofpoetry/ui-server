open Storage.Database
open Board_qos_types
open Db_common
open Containers
open Common
open Printf

let ( % ) = Fun.( % )

let typ : (ID.t * T2mi_info.t Time.timespan) Caqti_type.t =
  let open T2mi_info in
  Types.custom
    Types.(
      List.(
        ID.db
        & option int
        & int
        & string
        & option string
        & option string
        & bool
        & bool
        & ptime
        & ptime))
    ~encode:(fun (id, ({ from; till; data = sid, data } : t Time.timespan)) ->
      let packets =
        Json.(List.to_yojson Int.to_yojson) data.packets
        |> Yojson.Safe.to_string
      in
      let l1_pre, l1_post_conf =
        match data.l1 with
        | None -> (None, None)
        | Some x ->
            let to_string = Yojson.Safe.to_string in
            ( Some (to_string @@ l1_pre_to_yojson x.l1_pre),
              Some (to_string @@ l1_post_conf_to_yojson x.l1_post_conf) )
      in
      Ok
        ( ID.to_db id,
          ( data.t2mi_pid,
            ( sid,
              ( packets,
                ( l1_pre,
                  ( l1_post_conf,
                    (data.l1_empty, (data.l1_parse_error, (from, till))) ) ) )
            ) ) ))
    ~decode:
      (fun ( id,
             ( t2mi_pid,
               ( sid,
                 ( packets,
                   ( l1_pre,
                     (l1_post, (l1_empty, (l1_parse_error, (from, till)))) ) )
               ) ) ) ->
      let open Option in
      let of_ f = Option.of_result % f % Yojson.Safe.from_string in
      let packets =
        Json.(List.of_yojson Int.of_yojson) @@ Yojson.Safe.from_string packets
      in
      let l1_pre = flat_map (of_ l1_pre_of_yojson) l1_pre in
      let l1_post = flat_map (of_ l1_post_conf_of_yojson) l1_post in
      match (packets, l1_pre, l1_post) with
      | Ok packets, Some l1_pre, Some l1_post_conf ->
          let data =
            {
              t2mi_pid;
              packets;
              l1 = Some { l1_pre; l1_post_conf };
              l1_empty;
              l1_parse_error;
            }
          in
          Ok (ID.of_db id, { from; till; data = (sid, data) })
      | _ -> Error "json parse error")

let insert db (data : (ID.t * T2mi_info.t Time.timespan list) list) =
  let table = (Conn.names db).t2mi_info in
  let data =
    List.map (fun (id, x) -> List.map (Pair.make id) x) data |> List.concat
  in
  let insert =
    R.exec typ
      (sprintf
         {|INSERT INTO %s (%s) VALUES (%s)
                ON CONFLICT DO NOTHING|}
         table
         (to_columns_string Model.keys_t2mi_info)
         (to_values Model.keys_t2mi_info))
  in
  Conn.request db
    Request.(
      with_trans
        (List.fold_left
           (fun acc v -> acc >>= fun () -> exec insert v)
           (return ()) data))

let bump db (data : (ID.t * T2mi_info.t Time.timespan list) list) =
  let table = (Conn.names db).t2mi_info in
  let data =
    List.map
      (fun (id, x) ->
        List.map
          (fun ({ data = sid, _; till; _ } : T2mi_info.t Time.timespan) ->
            (ID.to_db id, sid, till))
          x)
      data
    |> List.concat
  in
  let update_last =
    R.exec
      Types.(tup3 ID.db int ptime)
      (sprintf
         {|UPDATE %s SET date_end = $3
                WHERE stream = $1
                AND t2mi_stream_id = $2
                AND date_start = (SELECT date_start FROM %s
                WHERE stream = $1 AND t2mi_stream_id = $2
                ORDER BY date_start DESC LIMIT 1)|}
         table table)
  in
  Conn.request db
    Request.(
      with_trans
        (List.fold_left
           (fun acc s -> acc >>= fun () -> exec update_last s)
           (return ()) data))
