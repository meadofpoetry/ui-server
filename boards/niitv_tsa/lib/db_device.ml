open Containers
open Storage.Database
open Board_qos_types
open Printf
open Db_common
open Common
open Api.Api_types
open Lwt.Infix

type state = Common.Topology.state

let state_to_int = function `Fine -> 0 | `Init -> 1 | `No_response -> 2
let state_of_int = function 0 -> `Fine | 1 -> `Init | _ -> `No_response

let init db =
  let open Printf in
  let table = (Conn.names db).state in
  let insert_new =
    R.exec Types.(tup3 int ptime ptime)
      (sprintf "INSERT INTO %s (state,date_start,date_end)
                VALUES (?,?,?)" table) in
  let now = Ptime_clock.now () in
  let new_state = state_to_int `No_response in
  Conn.request db Request.(exec insert_new (new_state, now, now))

let bump db state =
  let open Printf in
  let table = (Conn.names db).state in
  let select_last =
    R.find_opt Types.unit Types.(tup3 int ptime ptime)
      (sprintf "SELECT * FROM %s
                ORDER BY date_end DESC LIMIT 1" table) in
  let update_last =
    R.exec Types.(tup3 ptime ptime ptime)
      (sprintf "UPDATE %s SET date_end = ?
                WHERE date_start = ? AND date_end = ?" table)
  (* TODO optimize out*) in
  let insert_new =
    R.exec Types.(tup3 int ptime ptime)
      (sprintf "INSERT INTO %s (state,date_start,date_end)
                VALUES (?,?,?)" table) in
  let now = Ptime_clock.now () in
  let new_state = state_to_int state in
  Conn.request db Request.(
    with_trans (find select_last () >>= function
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
    list select (from, till, limit) >>= fun l ->
    let data =
      List.map (fun (st, from, till) ->
          { state = state_of_int st; from; till }) l in
    return (Raw { data
                ; has_more = (List.length data >= limit)
                ; order = `Desc }))

(* TODO fix it *)
let select_state_compressed_internal db ~from ~till =
  let table = (Conn.names db).state in
  let dif   = Time.(Span.to_float_s @@ diff till from) in
  let select_i =
    R.collect Types.(tup2 ptime ptime) Types.(tup2 int ptime_span)
      (sprintf {|SELECT state, sum(date_end - date_start) FROM %s
                WHERE date_start <= $2 AND date_start >= $1
                AND date_end <= $2 AND date_end >= $1
                GROUP BY state|} table) in
  let select_l =
    R.find_opt Types.(tup2 ptime ptime) Types.(tup2 int ptime)
      (sprintf {|SELECT state, max(date_end) FROM %s
                WHERE date_start < $1 AND date_end <= $2 AND date_end >= $1
                GROUP BY state LIMIT 1|} table) in
  let select_r =
    R.find_opt Types.(tup2 ptime ptime) Types.(tup2 int ptime)
      (sprintf {|SELECT state, min(date_start) FROM %s
                WHERE date_end > $2 AND date_start <= $2 AND date_start >= $1
                GROUP BY state LIMIT 1|} table) in
  let select_o =
    R.find_opt Types.(tup2 ptime ptime) Types.int
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
       find select_l (from, till) >>= fun l ->
       find select_r (from, till) >>= fun r ->
       list select_i (from, till) >>= fun i ->
       let l = match l with
         | None -> fun x -> x
         | Some (st, t) ->
            let sp = Time.diff t from in
            fun (s,v) -> if Pervasives.(=) s st then (s,Time.Span.add sp v)
                         else (s,v) in
       let r = match r with
         | None -> fun x -> x
         | Some (st, t) ->
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
