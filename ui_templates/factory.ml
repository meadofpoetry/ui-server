open Containers
open Components
open Lwt_result.Infix

let return = Lwt_result.return

module Factory_state = struct

  type 'a t =
    { mutable value     : 'a option
    ; mutable fin       : (unit -> unit)
    ; mutable ref_count : int
    }

  type 'a value_lwt = ('a,string) Lwt_result.t

  type 'a t_lwt = 'a value_lwt t

  type ws = WebSockets.webSocket Js.t

  let finalize (state:'a t) = state.fin (); state.value <- None; state.ref_count <- 0

  let succ_ref (state:'a t) : unit =
    state.ref_count <- succ state.ref_count

  let pred_ref ?(fin=false) (state:'a t) : unit =
    state.ref_count <- pred state.ref_count;
    if state.ref_count <= 0 && fin then finalize state

  let zero_ref (state:'a t) : unit =
    state.ref_count <- 0

  let set_ref (state:'a t) (v:int) =
    state.ref_count <- v

  let empty () = { value = None
                 ; fin = (fun () -> ())
                 ; ref_count = 0
                 }

end

module Factory_state_lwt = struct

  type 'a value = ('a,string) Lwt_result.t
  type 'a t     = ('a value) Factory_state.t

  let l1 : 'a. 'a value -> ('a -> 'b) -> 'b value = fun t1 f ->
    t1 >>= fun t1 -> Lwt_result.return @@ f t1

  let l2 : 'a 'b. 'a value -> 'b value -> ('a -> 'b -> 'c) -> 'c value = fun t1 t2 f ->
    t1 >>= fun t1 -> t2 >>= (fun t2 -> Lwt_result.return @@ f t1 t2)

  let l3 : 'a 'b 'c. 'a value -> 'b value -> 'c value -> ('a -> 'b -> 'c -> 'd) -> 'd value = fun t1 t2 t3 f ->
    t1 >>= fun t1 -> t2 >>= fun t2 -> t3 >>= (fun t3 -> Lwt_result.return @@ f t1 t2 t3)

  let get_value_as_signal ~(get:(unit -> 'a value))
                          ~(get_socket:(unit -> 'a React.event * Factory_state.ws))
                          (state:('a React.signal t)) : 'a React.signal value =
    match state.value with
    | Some x -> Factory_state.succ_ref state; x
    | None   -> Factory_state.set_ref state 1;
                let t = get ()
                        >>= (fun state -> let e,sock = get_socket () in
                                          let s      = React.S.hold state e in
                                          let fin () = sock##close;
                                                       React.E.stop ~strong:true e;
                                                       React.S.stop ~strong:true s
                                          in
                                          return (s,fin))
                in
                let v = t >>= (fun (s,_) -> return s) in
                state.value <- Some v;
                state.fin   <- (fun () -> t >>= (fun (_,f) -> return @@ f ()) |> Lwt.ignore_result);
                v

end
