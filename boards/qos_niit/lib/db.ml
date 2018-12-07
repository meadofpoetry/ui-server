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


module Data_handler = struct
  open Common
  open Lwt.Infix

  module Coll = struct

    type 'a coll = 'a list timestamped
    type 'a flat = 'a timespan list
    type 'a t =
      { lost : (Stream.ID.t * 'a flat) list
      ; found : (Stream.ID.t * 'a flat) list
      ; upd : (Stream.ID.t * 'a flat) list
      }

    module I = struct

      type 'a t =
        { lost : 'a flat
        ; found : 'a flat
        ; upd : 'a flat
        }

      let flatten ?till (c : 'a coll) : 'a flat =
        let till = match till with
          | Some x -> x
          | None -> c.timestamp in
        List.map (make_timespan ~from:c.timestamp ~till) c.data

      let split ~(eq : 'a -> 'a -> bool)
            (past : (Stream.ID.t * 'a flat) list)
            (id, (data : 'a coll)) =
        match List.Assoc.get ~eq:Stream.ID.equal id past with
        | None -> id, { lost = []; upd = []; found = flatten data }
        | Some past_data ->
           let (lost : 'a flat) =
             List.filter_map (fun (x : 'a timespan) ->
                 if List.mem ~eq x.data data.data then None else
                   Some { x with till = data.timestamp }) past_data in
           let (found : 'a flat), (upd : 'a flat) =
             List.fold_left (fun (acc_new, acc_upd) (x : 'a) ->
                 match List.find_opt (fun (i : 'a timespan) -> eq x i.data)
                         past_data with
                 | Some p ->
                    let i = { p with till = data.timestamp } in
                    (acc_new, i :: acc_upd)
                 | None ->
                    let till = data.timestamp in
                    let from = data.timestamp in
                    let i = make_timespan ~from ~till x in
                    (i :: acc_new, acc_upd)) ([], []) data.data in
           (id, { lost; found; upd })

    end

    let empty =
      { lost = []; found = []; upd = [] }

    let flatten ?till x =
      List.map (fun (id, x) ->
          id, I.flatten ?till x) x

    let split ~(eq : 'a -> 'a -> bool)
          (pres : (Stream.ID.t * 'a coll) list)
          (past : (Stream.ID.t * 'a flat) list) : 'a t =
      let (l : (Stream.ID.t * 'a I.t) list) =
        List.map (I.split ~eq past) pres in
      let lost, upd, found =
        List.fold_left (fun (lost_acc, found_acc, upd_acc)
                            (id, ({ lost; found; upd } : 'a I.t)) ->
            let lost = match lost with
              | [] -> lost_acc
              | x -> (id, x) :: lost_acc in
            let found = match found with
              | [] -> found_acc
              | x -> (id, x) :: found_acc in
            let upd = match upd with
              | [] -> upd_acc
              | x -> (id, x) :: upd_acc in
            (lost, upd, found)) ([], [], []) l
      in
      { lost; found; upd }

    let handle ~eq ~insert ~bump db tick s =
      let open React in
      let open E in
      S.changes s
      |> fold (fun { upd; found; _ } n ->
             split ~eq n (upd @ found)) empty
      |> (fun e ->
        select [e; S.sample (fun () e ->
                       let upd = flatten ~till:(Ptime_clock.now ()) e in
                       { upd; lost = []; found = [] }) tick s])
      |> map_s (fun { lost; upd; found } ->
             let t = if List.is_empty lost then Lwt.return_unit
                     else bump db lost in
             t
             >>= (fun () ->
               if List.is_empty upd then Lwt.return_unit
               else bump db upd)
             >>= (fun () ->
               if List.is_empty found then Lwt.return_unit
               else insert db found))
      |> keep

  end

  module Single = struct

    type 'a t =
      { lost : (Stream.ID.t * 'a timespan) list
      ; found : (Stream.ID.t * 'a timespan) list
      ; upd : (Stream.ID.t * 'a timespan) list
      }

    let empty =
      { lost = []; found = []; upd = [] }

    let split ~(eq : 'a -> 'a -> bool)
          (pres : (Stream.ID.t * 'a timestamped) list)
          (past : (Stream.ID.t * 'a timespan) list) : 'a t =
      let _ = eq in
      ignore pres; ignore past;
      empty

    let handle ~eq ~insert ~bump db tick s =
      let _ = tick in
      let _ = eq in
      let open React in
      let open E in
      S.changes s
      |> fold (fun { upd; found; _ } n ->
             split ~eq:Board_types.Ts_info.equal n (upd @ found)) empty
      |> (fun e -> select [e; e])
      |> map_s (fun { lost; upd; found } ->
             let t = if List.is_empty lost then Lwt.return_unit
                     else bump db lost in
             t
             >>= (fun () ->
               if List.is_empty upd then Lwt.return_unit
               else bump db upd)
             >>= (fun () ->
               if List.is_empty found then Lwt.return_unit
               else insert db found))
      |> keep

  end

end
