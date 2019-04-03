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
  type ('a, 'b) pending = ['b rsp | `Init] Lwt.t * 'a resolver
  type ('a, 'b) t = ('a, 'b) pending * ('a, 'b) msg list

  let create l = (Lwt.return `Init, fun _ -> ()), l

  let apply (t : ('a, 'b) t) (l : 'a list) =
    (snd (fst t)) (`Msgs l); t

  let append (hd, tl) m : ('a, 'b) t =
    hd, List.append tl m

  let is_empty = function _, [] -> true | _ -> false

  let send (t : ('a, 'b) t) : ('a, 'b) t =
    match snd t with
    | [] -> t
    | f :: tl -> (f () :> ('a, 'b) pending), tl

  let _match (t : ('a, 'b) t) ~resolved ~timeout ~pending ~not_sent =
    match Lwt.state (fst (fst t)) with
    | Lwt.Return `Init -> not_sent t
    | Lwt.Return `Tm -> timeout t
    | Lwt.Return `V x -> resolved t x
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
