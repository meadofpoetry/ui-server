exception Full

exception Empty

type 'a t =
  { m : Lwt_mutex.t
  ; c : unit Lwt_condition.t
  ; mutable v : 'a list
  ; mutable pending : 'a option
  ; mutable push_wakener : unit Lwt.u
  ; mutable push_waiter : unit Lwt.t
  ; limit : int
  }

let ( >>= ) = Lwt.( >>= )

let length t = List.length t.v

let enqueue v (t : 'a t) =
  t.v <- v :: t.v

let notify_pusher (t : 'a t) =
  (match t.pending with
   | None -> ()
   | Some x -> enqueue x t);
  t.pending <- None;
  let prev_wakener = t.push_wakener in
  let waiter, wakener = Lwt.task () in
  t.push_waiter <- waiter;
  t.push_wakener <- wakener;
  Lwt.wakeup_later prev_wakener ()

let push x t =
  match t.pending with
  | Some _ -> Lwt.fail Full
  | None ->
    if List.length t.v >= t.limit
    then (
      t.pending <- Some x;
      Lwt.catch (fun () -> t.push_waiter)
        (function
          | Lwt.Canceled as exn ->
            t.pending <- None;
            let push_waiter, push_wakener = Lwt.task () in
            t.push_waiter <- push_waiter;
            t.push_wakener <- push_wakener;
            Lwt.fail exn
          | exn -> Lwt.fail exn))
    else (
      enqueue x t;
      Lwt_condition.signal t.c ();
      Lwt.return ())

let clear (t : 'a t) =
  (match t.pending with
   | None -> ()
   | Some _ ->
     t.pending <- None;
     Lwt.cancel t.push_waiter;
     let waiter, wakener = Lwt.task () in
     t.push_waiter <- waiter;
     t.push_wakener <- wakener);
  Lwt_mutex.lock t.m
  >>= fun () -> t.v <- []; Lwt_mutex.unlock t.m; Lwt.return ()

let create limit =
  let push_waiter, push_wakener = Lwt.task () in
  let info =
    { m = Lwt_mutex.create ()
    ; c = Lwt_condition.create ()
    ; v = []
    ; pending = None
    ; push_waiter
    ; push_wakener
    ; limit
    } in
  info, fun x -> push x info

let next t =
  Lwt_mutex.lock t.m
  >>= fun () ->
  (match t.v with
   | [] -> Lwt_condition.wait ~mutex:t.m t.c
   | _ -> Lwt.return ())
  >>= fun () ->
  match List.rev t.v with
  | [] -> Lwt.fail Empty
  | hd :: tl ->
    t.v <- List.rev tl;
    (match t.pending with
     | None -> ()
     | Some _ -> notify_pusher t);
    Lwt_mutex.unlock t.m;
    Lwt.return hd

let get_while f t =
  Lwt_mutex.lock t.m
  >>= fun () ->
  (match t.v with
   | [] -> Lwt_condition.wait ~mutex:t.m t.c
   | _ -> Lwt.return ())
  >>= fun () ->
  match t.v with
  | [] -> Lwt.fail Empty
  | v ->
    let l, r = List.partition f v in
    t.v <- r;
    (match l, t.pending with
     | [], _ | _, None -> ()
     | _ -> notify_pusher t);
    Lwt_mutex.unlock t.m;
    Lwt.return @@ List.rev l
