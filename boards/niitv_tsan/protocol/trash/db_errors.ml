open Containers
open Storage.Database
open Board_qos_types
open Printf
open Db_common
open Common
open Api.Api_types
open Lwt.Infix

let error =
  Types.custom
    Types.(
      List.(
        ID.db
        & int
        & int
        & int
        & int
        & bool
        & int
        & int32
        & int32
        & int32
        & ptime))
    ~encode:(fun (id, (err : Error.t)) ->
      Ok
        ( ID.to_db id,
          ( err.count,
            ( err.err_code,
              ( err.err_ext,
                ( err.priority,
                  ( err.multi_pid,
                    ( err.pid,
                      (err.packet, (err.param_1, (err.param_2, err.time))) ) )
                ) ) ) ) ))
    ~decode:
      (fun ( id,
             ( count,
               ( err_code,
                 ( err_ext,
                   ( priority,
                     (multi_pid, (pid, (packet, (param_1, (param_2, time))))) )
                 ) ) ) ) ->
      let (error : Error.t) =
        {
          count;
          err_code;
          err_ext;
          priority;
          multi_pid;
          pid;
          packet;
          param_1;
          param_2;
          time;
        }
      in
      Ok (ID.of_db id, error))

let insert db ~is_ts errs =
  let table = (Conn.names db).errors in
  let errs =
    List.map (fun ((id : Stream.ID.t), e) -> List.map (Pair.make id) e) errs
    |> List.concat
  in
  let insert =
    R.exec
      Types.(tup2 bool error)
      (sprintf
         {|INSERT INTO %s(is_ts,stream,count,err_code,err_ext,priority,
                multi_pid,pid,packet,param_1,param_2,date)
                VALUES (?,?,?,?,?,?,?,?,?,?,?,?)|}
         table)
  in
  Conn.request db
    Request.(
      with_trans
        (List.fold_left
           (fun acc x -> acc >>= fun () -> exec insert (is_ts, x))
           (return ()) errs))

let select_has_any db ?(streams = []) ?(priority = []) ?(pids = [])
    ?(errors = []) ~is_ts ~from ~till () =
  let table = (Conn.names db).errors in
  let streams = is_in "stream" ID.to_value_string streams in
  let priority = is_in "priority" string_of_int priority in
  let pids = is_in "pid" string_of_int pids in
  let errors = is_in "err_code" string_of_int errors in
  let select =
    R.find
      Types.(tup3 bool ptime ptime)
      Types.bool
      (sprintf
         {|SELECT EXISTS (SELECT 1 FROM %s
                WHERE %s %s %s %s is_ts = ? AND date >= ? AND date <= ?)|}
         table streams priority pids errors)
  in
  Conn.request db
    Request.(
      find select (is_ts, from, till) >>= function
      | None -> return false
      | Some x -> return x)

let select_percent db ?(streams = []) ?(priority = []) ?(pids = [])
    ?(errors = []) ~is_ts ~from ~till () =
  let table = (Conn.names db).errors in
  let streams = is_in "stream" ID.to_value_string streams in
  let priority = is_in "priority" string_of_int priority in
  let pids = is_in "pid" string_of_int pids in
  let errors = is_in "err_code" string_of_int errors in
  let span = Time.(Period.to_float_s @@ diff till from) in
  let select =
    R.find
      Types.(tup3 bool ptime ptime)
      Types.int
      (sprintf
         {|SELECT count(DISTINCT date_trunc('second',date)) FROM %s 
                WHERE %s %s %s %s is_ts = ? AND date >= ? AND date <= ?|}
         table streams priority pids errors)
  in
  Conn.request db
    Request.(
      find select (is_ts, from, till) >>= function
      | None -> return 0.0
      | Some s -> return (100. *. float_of_int s /. span))

let select_errors db ?(streams = []) ?(priority = []) ?(pids = [])
    ?(errors = []) ?(limit = 500) ?(order = `Desc) ~is_ts ~from ~till () =
  let table = (Conn.names db).errors in
  let streams = is_in "stream" ID.to_value_string streams in
  let priority = is_in "priority" string_of_int priority in
  let pids = is_in "pid" string_of_int pids in
  let errors = is_in "err_code" string_of_int errors in
  let ord = match order with `Desc -> "DESC" | `Asc -> "ASC" in
  let select =
    R.collect
      Types.(tup4 bool ptime ptime int)
      error
      (sprintf
         {|SELECT stream,count,err_code,err_ext,priority,
                multi_pid,pid,packet,param_1,param_2,date
                FROM %s
                WHERE %s %s %s %s is_ts = ? AND date >= ? AND date <= ?
                ORDER BY date %s LIMIT ?|}
         table streams priority pids errors ord)
  in
  Conn.request db
    Request.(
      list select (is_ts, from, till, limit) >>= fun data ->
      let (data : (ID.t * Error.t list) list) =
        List.fold_left
          (fun acc (id, x) ->
            List.Assoc.update ~eq:Stream.ID.equal
              (function None -> Some [ x ] | Some l -> Some (x :: l))
              id acc)
          [] data
      in
      return (Raw { data; has_more = List.length data >= limit; order }))

(* TODO consider stream presence *)
let select_errors_compressed db ?(streams = []) ?(priority = []) ?(pids = [])
    ?(errors = []) ~is_ts ~from ~till () =
  let table = (Conn.names db).errors in
  let streams = is_in "stream" ID.to_value_string streams in
  let priority = is_in "priority" string_of_int priority in
  let pids = is_in "pid" string_of_int pids in
  let errors = is_in "err_code" string_of_int errors in
  let intvals = Time.split ~from ~till in
  let select =
    R.find
      Types.(tup3 bool ptime ptime)
      Types.int
      (sprintf
         {|SELECT count(DISTINCT date_trunc('second',date)) FROM %s
                WHERE %s %s %s %s is_ts = ? AND date >= ? AND date <= ?|}
         table streams priority pids errors)
  in
  Conn.request db
    Request.(
      with_trans
        (List.fold_left
           (fun acc (f, t) ->
             let span = Time.(Period.to_float_s @@ diff till from) in
             acc >>= fun l ->
             find select (is_ts, f, t) >>= function
             | None -> return l
             | Some s ->
                 let perc = 100. *. float_of_int s /. span in
                 return ((perc, f, t) :: l))
           (return []) intvals))
  >>= fun l ->
  List.fold_left
    (fun acc (p, from, till) ->
      acc >>= fun acc ->
      Db_device.select_state_compressed_internal db ~from ~till
      >>= fun { fine; _ } ->
      let data =
        Error.{ errors = 100. *. p /. fine; no_stream = 100. -. fine }
      in
      Lwt.return (Time.{ from; till; data } :: acc))
    (Lwt.return []) l
  >|= fun data -> Compressed { data }
