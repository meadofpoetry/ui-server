open Application_types
open Board_niitv_tsan_types

type acc =
  { min : int
  ; max : int
  ; cur : int }

let value_of_acc (acc : acc) : Bitrate.value =
  {min = acc.min; max = acc.max; cur = acc.cur}

type t =
  { hashtbl : (Stream.ID.t, int Bitrate.t Queue.t) Hashtbl.t
  ; mutable period : Ptime.span }

let set_period t span = t.period <- span

let map_bitrate (x : Bitrate.cur) : Bitrate.ext =
  let value_of_int (cur : int) : Bitrate.value = {min = cur; max = cur; cur} in
  { total = value_of_int x.total
  ; effective = value_of_int x.effective
  ; pids = List.map (fun (pid, v) -> pid, value_of_int v) x.pids
  ; tables = List.map (fun (id, v) -> id, value_of_int v) x.tables
  ; timestamp = x.timestamp }

let clear_stream t stream =
  match Hashtbl.find_opt t.hashtbl stream with
  | None -> ()
  | Some queue -> Queue.clear queue

let clear x = Hashtbl.clear x.hashtbl

let create ?(period = Ptime.Span.of_int_s (5 * 60)) () =
  {hashtbl = Hashtbl.create 50; period}

let update_acc (acc : acc) (v : int) = {acc with min = min acc.min v; max = max acc.max v}

let push_queue (period : Ptime.span) (queue : int Bitrate.t Queue.t) (v : int Bitrate.t)
    =
  if Queue.is_empty queue
  then Queue.push v queue
  else
    let old = (Queue.peek queue).timestamp in
    let now = v.timestamp in
    Queue.push v queue;
    if Ptime.diff now old > period then ignore @@ Queue.take queue

let fold (queue : Bitrate.cur Queue.t) (v : Bitrate.cur) : Bitrate.ext =
  let acc_of_int x = {min = x; max = x; cur = x} in
  let total, effective, pids, tables =
    Queue.fold
      (fun (tot, eff, pid, tbl) (x : int Bitrate.t) ->
        let total = update_acc tot x.total in
        let effective = update_acc eff x.effective in
        let pids =
          List.map
            (fun (pid, acc) ->
              match List.assoc_opt pid x.pids with
              | None -> pid, acc
              | Some v -> pid, update_acc acc v)
            pid
        in
        let tables =
          List.map
            (fun (id, acc) ->
              match List.assoc_opt id x.tables with
              | None -> id, acc
              | Some v -> id, update_acc acc v)
            tbl
        in
        total, effective, pids, tables)
      ( acc_of_int v.total
      , acc_of_int v.effective
      , List.map (fun (pid, v) -> pid, acc_of_int v) v.pids
      , List.map (fun (id, v) -> id, acc_of_int v) v.tables )
      queue
  in
  { total = value_of_acc total
  ; effective = value_of_acc effective
  ; pids = List.map (fun (pid, acc) -> pid, value_of_acc acc) pids
  ; tables = List.map (fun (id, acc) -> id, value_of_acc acc) tables
  ; timestamp = v.timestamp }

let map (t : t) (bitrate : (Stream.ID.t * int Bitrate.t) list) =
  List.map
    (fun (id, (bitrate : int Bitrate.t)) ->
      match Hashtbl.find_opt t.hashtbl id with
      | None ->
          let queue = Queue.create () in
          Queue.add bitrate queue;
          Hashtbl.add t.hashtbl id queue;
          id, map_bitrate bitrate
      | Some queue ->
          push_queue t.period queue bitrate;
          id, fold queue bitrate)
    bitrate
