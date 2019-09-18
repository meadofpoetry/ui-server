open Application_types
open Board_niitv_tsan_types

type t =
  { hashtbl : (Stream.ID.t, int Bitrate.t Queue.t) Hashtbl.t
  ; mutable period : Ptime.span }

let set_period t span = t.period <- span

let clear_stream t stream =
  match Hashtbl.find_opt t.hashtbl stream with
  | None -> ()
  | Some queue -> Queue.clear queue

let clear x = Hashtbl.clear x.hashtbl

let create ?(period = Ptime.Span.of_int_s (5 * 60)) () =
  {hashtbl = Hashtbl.create 50; period}

let update_acc (acc : Bitrate.value) (v : int) =
  {acc with min = min acc.min v; max = max acc.max v}

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
  Queue.fold
    (fun (acc : Bitrate.ext) (x : int Bitrate.t) ->
      { acc with
        total = update_acc acc.total x.total
      ; effective = update_acc acc.effective x.effective
      ; pids =
          List.map
            (fun (pid, acc) ->
              match List.assoc_opt pid x.pids with
              | None -> pid, acc
              | Some v -> pid, update_acc acc v)
            acc.pids
      ; tables =
          List.map
            (fun (id, acc) ->
              match List.assoc_opt id x.tables with
              | None -> id, acc
              | Some v -> id, update_acc acc v)
            acc.tables })
    (Bitrate.cur_to_ext v)
    queue

let map (t : t) (bitrate : (Stream.ID.t * int Bitrate.t) list) =
  List.map
    (fun (id, (bitrate : int Bitrate.t)) ->
      match Hashtbl.find_opt t.hashtbl id with
      | None ->
          let queue = Queue.create () in
          Queue.add bitrate queue;
          Hashtbl.add t.hashtbl id queue;
          id, Bitrate.cur_to_ext bitrate
      | Some queue ->
          push_queue t.period queue bitrate;
          id, fold queue bitrate)
    bitrate
