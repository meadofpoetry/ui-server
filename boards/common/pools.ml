type 'a rsp = [`V of 'a | `Tm]
type 'a resolver = [`Tm | `Msgs of 'a list] -> unit
type ('a, 'b) msg = unit -> ('b rsp Lwt.t * 'a resolver)

module Pool = struct

  type ('a, 'b) pending = ['b rsp | `Init] Lwt.t * 'a resolver
  type ('a, 'b) aux =
    { curs : int
    ; reqs : ('a, 'b) msg array
    }
  type ('a, 'b) t = ('a, 'b) pending * ('a, 'b) aux

  let create l =
    (Lwt.return `Init, (fun _ -> ())),
    { curs = 0; reqs = Array.of_list l }

  let apply t l =
    let resolver = snd (fst t) in
    resolver (`Msgs l);
    t

  let is_empty (t : ('a, 'b) t) : bool =
    Array.length (snd t).reqs = 0

  let is_last (t : ('a, 'b) t) : bool =
    (snd t).curs = (Array.length (snd t).reqs) - 1

  let send (t : ('a, 'b) t) : ('a, 'b) t =
    let v = snd t in
    (v.reqs.(v.curs) () :> ('a, 'b) pending),
    { v with curs = (succ v.curs) mod (Array.length v.reqs) }

  let _match (t : ('a, 'b) t) ~resolved ~timeout ~pending ~not_sent =
    match Lwt.state (fst (fst t)) with
    | Lwt.Return `Init -> not_sent t
    | Lwt.Return `Tm -> timeout t
    | Lwt.Return `V x -> resolved t x
    | Lwt.Sleep -> pending t
    | Lwt.Fail e -> Lwt.fail e
end

module Queue = struct
  let ( >>= ) = Lwt.bind

  type e = [`Timeout | `Interrupted]

  type ('a, 'b) msg =
    { timeout : unit -> unit Lwt.t
    ; send : unit -> unit Lwt.t
    ; resolve : 'a -> 'b option
    }

  type ('a, 'b) msg_internal =
    { msg : ('a, 'b) msg
    ; waiter : ('b, e) result Lwt.t
    ; wakener : ('b, e) result Lwt.u
    ; timer : unit Lwt.t
    }

  type ('a, 'b) t =
    { pending : ('a, 'b) msg_internal option
    ; msgs : ('a, 'b) msg_internal list
    }

  let make_msg ~send ~timeout ~resolve () : ('a, 'b) msg =
    { timeout
    ; send
    ; resolve
    }

  let make_msg_internal (msg : ('a, 'b) msg) : ('a, 'b) msg_internal =
    let t, w = Lwt.wait () in
    { msg; waiter = t; wakener = w; timer = Lwt.return () }

  let invalidate (t : ('a, 'b) t) : ('a, 'b) t =
    List.iter (fun (m : ('a, 'b) msg_internal) ->
        Lwt.wakeup_later m.wakener (Error `Interrupted)) t.msgs;
    (match t.pending with
     | None -> ()
     | Some { waiter; wakener; _ } ->
        match Lwt.state waiter with
        | Lwt.Sleep -> Lwt.wakeup_later wakener (Error `Interrupted)
        | _ -> ());
    { msgs = []; pending = None }

  let create l =
    let msgs = List.map make_msg_internal l in
    { pending = None; msgs }

  let apply (t : ('a, 'b) t) (l : 'a list) =
    match t.pending with
    | None -> ()
    | Some { waiter; wakener; msg } ->
       match Lwt.state waiter with
       | Lwt.Return _ | Lwt.Fail _ -> ()
       | Lwt.Sleep ->
          match Util.List.find_map msg.resolve l with
          | None -> ()
          | Some x -> Lwt.wakeup_later wakener (Ok x)

  let is_empty t = match t.msgs with [] -> true | _ -> false

  let snoc t msg =
    let msg' = make_msg_internal msg in
    msg'.waiter, { t with msgs = t.msgs @ [msg'] }

  let cons t msg =
    let msg' = make_msg_internal msg in
    msg'.waiter, { t with msgs = msg' :: t.msgs }

  let append t msgs =
    let msgs' = List.map make_msg_internal msgs in
    { t with msgs = t.msgs @ msgs' }

  let send (t : ('a, 'b) t) : ('a, 'b) t Lwt.t =
    match t.msgs with
    | [] -> Lwt.return t
    | m :: tl ->
       m.msg.send ()
       >>= fun () ->
       let timer =
         m.msg.timeout ()
         >>= fun () ->
         match Lwt.state m.waiter with
         | Lwt.Sleep -> Lwt.return @@ Lwt.wakeup_later m.wakener (Error `Timeout)
         | Lwt.Fail e -> Lwt.fail e
         | Lwt.Return _ -> Lwt.return () in
       let pending = Some { m with timer } in
       Lwt.return { pending; msgs = tl }

  let _match (t : ('a, 'b) t) ~resolved ~error ~pending ~not_sent =
    match t.pending with
    | None -> not_sent t
    | Some { waiter; _ } ->
       match Lwt.state waiter with
       | Lwt.Return (Ok x) -> resolved t x
       | Lwt.Return (Error e) -> error t e
       | Lwt.Sleep -> pending t
       | Lwt.Fail e -> Lwt.fail e
end

module Await_queue = struct
  type ('a, 'b) pending = ['b rsp | `Init] Lwt.t * 'a resolver
  type ('a, 'b) t = ('a, 'b) pending list * ('a, 'b) msg list

  let is_empty = function _, [] -> true | _ -> false

  let has_pending = function [], _ -> false | _ -> true

  let create l = [], l

  let apply (t : ('a, 'b) t) (l : 'a list) =
    List.iter (fun (_, resolve) -> resolve (`Msgs l)) (fst t); t

  let send (t : ('a, 'b) t) : ('a, 'b) t =
    match snd t with
    | [] -> t
    | f :: tl -> (f () :> ('a, 'b) pending) :: (fst t), tl
end
