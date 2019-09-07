exception Full

exception Empty

(* TODO replace list with smth more efficient *)

type 'a t =
  { m : Lwt_mutex.t
  ; c : [`V of 'a * 'a list | `E] Lwt_condition.t
  ; pred : 'a -> bool
  ; mutable v : 'a list
  ; mutable pending : 'a option
  ; mutable push_wakener : unit Lwt.u
  ; mutable push_waiter : unit Lwt.t
  ; limit : int }

let ( >>= ) = Lwt.( >>= )

let length t = List.length t.v

let enqueue v (t : 'a t) = t.v <- t.v @ [v]

let notify_pusher (t : 'a t) =
  match t.pending with
  | None -> ()
  | Some x ->
      enqueue x t;
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
        Lwt.catch
          (fun () -> t.push_waiter)
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
        if t.pred x then Lwt_condition.signal t.c `E;
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
  Lwt_mutex.with_lock t.m (fun () ->
      t.v <- [];
      Lwt.return ())

let create limit pred =
  let push_waiter, push_wakener = Lwt.task () in
  let info =
    { m = Lwt_mutex.create ()
    ; c = Lwt_condition.create ()
    ; v = []
    ; pred
    ; pending = None
    ; push_waiter
    ; push_wakener
    ; limit }
  in
  info, fun x -> push x info

let check_condition t =
  Lwt_mutex.with_lock t.m (fun () ->
      if not @@ List.for_all (fun x -> not (t.pred x)) t.v
      then Lwt_condition.signal t.c `E;
      Lwt.return ())

let rec find_and_remove f = function
  | [] -> None, []
  | hd :: tl ->
      if f hd
      then Some hd, tl
      else
        let r, l = find_and_remove f tl in
        r, hd :: l

let next t =
  Lwt_mutex.with_lock t.m (fun () ->
      (match find_and_remove t.pred t.v with
      | None, _ -> Lwt_condition.wait ~mutex:t.m t.c
      | Some x, v -> Lwt.return (`V (x, v)))
      >>= function
      | `V (x, v) ->
          t.v <- v;
          notify_pusher t;
          Lwt.return x
      | `E -> (
          let x, l = find_and_remove t.pred t.v in
          t.v <- l;
          match x with
          | None -> Lwt.fail Empty
          | Some x ->
              notify_pusher t;
              Lwt.return x))
