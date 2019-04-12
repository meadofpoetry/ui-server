type ('a, 'b) msg =
  { timeout : unit -> unit Lwt.t
  ; send : unit -> unit Lwt.t
  ; resolve : 'a -> 'b option
  }

let make_msg ~send ~timeout ~resolve () : ('a, 'b) msg =
  { timeout
  ; send
  ; resolve
  }

type ('a, 'b, 'e) msg_internal =
  { msg : ('a, 'b) msg
  ; waiter : ('b, 'e) result Lwt.t
  ; wakener : ('b, 'e) result Lwt.u
  ; timer : unit Lwt.t
  }

let make_msg_internal (msg : ('a, 'b) msg) : ('a, 'b, 'e) msg_internal =
  let t, w = Lwt.wait () in
  { msg; waiter = t; wakener = w; timer = Lwt.return () }

module Pool = struct

  let ( >>= ) = Lwt.bind

  type e = [`Timeout]

  type ('a, 'b) t =
    { pending : ('a, 'b, e) msg_internal option
    ; current : int
    ; msgs : ('a, 'b, e) msg_internal array
    }

  let create l : ('a, 'b) t =
    let msgs = List.map make_msg_internal l in
    { pending = None
    ; current = 0
    ; msgs = Array.of_list msgs
    }

  let apply (t : ('a, 'b) t) (l : 'a list) =
    match t.pending with
    | None -> ()
    | Some { waiter; wakener; msg; timer } ->
       match Lwt.state waiter with
       | Lwt.Return _ | Lwt.Fail _ -> ()
       | Lwt.Sleep ->
          match Util.List.find_map msg.resolve l with
          | None -> ()
          | Some x ->
             Lwt.cancel timer;
             Lwt.wakeup_later wakener (Ok x)

  let append t msgs =
    let msgs' = List.map make_msg_internal msgs in
    { t with msgs = Array.append t.msgs (Array.of_list msgs') }

  let is_empty (t : ('a, 'b) t) : bool =
    Array.length t.msgs = 0

  let is_last (t : ('a, 'b) t) : bool =
    t.current = Array.length t.msgs - 1

  let send (t : ('a, 'b) t) : ('a, 'b) t Lwt.t =
    match t.msgs with
    | [||] -> Lwt.return t
    | msgs ->
       let m = msgs.(t.current) in
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
       Lwt.return { t with pending
                         ; current = (succ t.current) mod (Array.length msgs) }

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

module Queue = struct
  let ( >>= ) = Lwt.bind

  type e = [`Timeout | `Interrupted]

  type ('a, 'b) t =
    { pending : ('a, 'b, e) msg_internal option
    ; msgs : ('a, 'b, e) msg_internal list
    }

  let invalidate (t : ('a, 'b) t) : ('a, 'b) t =
    List.iter (fun (m : ('a, 'b, e) msg_internal) ->
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
