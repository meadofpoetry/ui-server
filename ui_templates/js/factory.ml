open Js_of_ocaml
open Containers
open Lwt_result.Infix

module State = struct

  type 'a t =
    { value : 'a
    ; fin : (unit -> unit)
    ; ref_count : int ref
    }

  let finalize (state : 'a t) =
    state.fin ();
    state.ref_count := 0

  let succ_ref (state : 'a t) : unit =
    Ref.update succ state.ref_count

  let pred_ref ?(fin = false) (state : 'a t) : unit =
    Ref.update pred state.ref_count;
    if !(state.ref_count) <= 0 && fin
    then finalize state

  let zero_ref (state : 'a t) : unit =
    state.ref_count := 0

  let set_ref (state : 'a t) (v : int) =
    state.ref_count := v

  let make_empty () =
    { value = None
    ; fin = (fun () -> ())
    ; ref_count = ref 0
    }

  let make ?(fin = (fun () -> ()))
        ?(ref_count = ref 0)
        value =
    { value; fin; ref_count }

end

module Lift = struct

  type ('a, 'b) t = ('a, 'b) Lwt_result.t

  let l1 : 'a. ('a, _) t -> ('a -> 'b) -> ('b, _) t =
    fun t1 f ->
    t1 >>= fun t1 -> Lwt_result.return @@ f t1

  let l2 : 'a 'b. ('a, _) t -> ('b, _) t -> ('a -> 'b -> 'c) -> ('c, _) t =
    fun t1 t2 f ->
    t1
    >>= fun t1 -> t2
    >>= fun t2 -> Lwt_result.return @@ f t1 t2

  let l3 = fun a b c f ->
    l2 a b (fun a b -> a, b)
    >>= fun (a, b) -> c
    >>= fun c -> Lwt_result.return @@ f a b c

  let l4 = fun a b c d f ->
    l3 a b c (fun a b c -> a, b, c)
    >>= fun (a, b, c) -> d
    >>= fun d -> Lwt_result.return @@ f a b c d

  let l5 = fun a b c d e f ->
    l4 a b c d (fun a b c d -> a, b, c, d)
    >>= fun (a, b, c, d) -> e
    >>= fun e -> Lwt_result.return @@ f a b c d e

  let l6 = fun a b c d e g f ->
    l5 a b c d e (fun a b c d e -> a, b, c, d, e)
    >>= fun (a, b, c, d, e) -> g
    >>= fun g -> Lwt_result.return @@ f a b c d e g

end

module Signal = struct

  open WebSockets

  type 'a t = ('a React.signal, string) Lwt_result.t State.t

  let make_state
        ~(get : (unit -> ('a, string) Lwt_result.t))
        ~(get_socket : (unit -> 'a React.event * webSocket Js.t)) : 'a t =
    let t =
      get ()
      >>= (fun state ->
        let e, sock = get_socket () in
        let s = React.S.hold state e in
        let state = (e, s, sock) in
        Lwt_result.return (s, state)) in
    let fin = fun () ->
      t >|= (fun (_, (e, s, sock)) ->
        sock##close;
        React.E.stop ~strong:true e;
        React.S.stop ~strong:true s)
      |> Lwt.ignore_result in
    let value = t >|= fst in
    State.make ~fin value

end
