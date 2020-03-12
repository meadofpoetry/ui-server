open Containers
open Storage.Database
open Board_qos_types
open Printf
open Db_common
open Common

let (typ : (ID.t * Service.t Time.timespan) Caqti_type.t) =
  Types.custom
    Types.(
      tup2
        (tup3 (tup2 ID.db int) (tup2 string string) (tup4 int int bool bool))
        (tup3 (tup4 bool bool bool bool) (tup4 bool int int int)
           (tup3 string ptime ptime)))
    ~encode:
      (fun (id, ({ from; till; data = sid, data } : Service.t Time.timespan)) ->
      let elements =
        Common.Json.(List.to_yojson Int.to_yojson) data.elements
        |> Yojson.Safe.to_string
      in
      Ok
        ( ( (ID.to_db id, sid),
            (data.name, data.provider_name),
            (data.pmt_pid, data.pcr_pid, data.has_pmt, data.has_sdt) ),
          ( (data.dscr, data.dscr_list, data.eit_schedule, data.eit_pf),
            ( data.free_ca_mode,
              data.running_status,
              data.service_type,
              data.service_type_list ),
            (elements, from, till) ) ))
    ~decode:
      (fun ( ( (id, sid),
               (name, provider_name),
               (pmt_pid, pcr_pid, has_pmt, has_sdt) ),
             ( (dscr, dscr_list, eit_schedule, eit_pf),
               (free_ca_mode, running_status, service_type, service_type_list),
               (elements, from, till) ) ) ->
      match
        Common.Json.(List.of_yojson Int.of_yojson)
        @@ Yojson.Safe.from_string elements
      with
      | Error e -> Error e
      | Ok elements ->
          Ok
            (let (data : Service.info) =
               {
                 name;
                 provider_name;
                 pmt_pid;
                 pcr_pid;
                 has_pmt;
                 has_sdt;
                 dscr;
                 dscr_list;
                 eit_schedule;
                 eit_pf;
                 free_ca_mode;
                 running_status;
                 service_type;
                 service_type_list;
                 elements;
               }
             in
             (ID.of_db id, { from; till; data = (sid, data) })))

let insert db (data : (ID.t * Service.t Time.timespan list) list) =
  let table = (Conn.names db).services in
  let data =
    List.map (fun (id, services) -> List.map (Pair.make id) services) data
    |> List.concat
  in
  let insert =
    R.exec typ
      (sprintf
         {|INSERT INTO %s (%s)
                VALUES (%s)
                ON CONFLICT DO NOTHING|}
         table
         (to_columns_string Model.keys_services)
         (to_values Model.keys_services))
  in
  Conn.request db
    Request.(
      with_trans
        (List.fold_left
           (fun acc v -> acc >>= fun () -> exec insert v)
           (return ()) data))

let bump db (data : (ID.t * Service.t Time.timespan list) list) =
  let table = (Conn.names db).services in
  let data =
    List.map
      (fun (id, services) ->
        List.map
          (fun ({ data = sid, _; till; _ } : Service.t Time.timespan) ->
            (ID.to_db id, sid, till))
          services)
      data
    |> List.concat
  in
  let update_last =
    R.exec
      Types.(tup3 ID.db int ptime)
      (sprintf
         {|UPDATE %s SET date_end = $3
                WHERE stream = $1
                AND id = $2
                AND date_start = (SELECT date_start FROM %s
                WHERE stream = $1 AND id = $2
                ORDER BY date_start DESC LIMIT 1)|}
         table table)
  in
  Conn.request db
    Request.(
      with_trans
        (List.fold_left
           (fun acc s -> acc >>= fun () -> exec update_last s)
           (return ()) data))
