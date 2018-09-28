open Containers
open Storage.Database
open Board_qos_types
open Printf

include Db_common

module Device = Db_device
module Streams = Db_streams
module Ts_info = Db_ts_info
module Services = Db_services
module Pids = Db_pids
module T2mi_info = Db_t2mi_info
module Errors = Db_errors

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
