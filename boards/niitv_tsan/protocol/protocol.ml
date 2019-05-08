open Board_niitv_tsan_types
open Application_types

type notifs =
  { state : Topology.state React.signal
  ; devinfo : devinfo option React.signal
  }

type api =
  { notifs : notifs
  ; kv : config Kv_v.rw
  ; channel : 'a. 'a Request.t -> ('a, Request.error) Lwt_result.t
  ; loop : unit -> unit Lwt.t
  ; push_data : Cstruct.t -> unit
  }

let msg_queue_size = 20

let ( >>= ) = Lwt.( >>= )

let await_no_response state =
  Util_react.(
    E.next
    @@ E.fmap (function
        | `Init | `No_response -> Some (Error Request.Not_responding)
        | `Fine -> None)
    @@ S.changes state)

let send (type a)
    (src : Logs.src)
    (state : Topology.state React.signal)
    (push : _ Lwt_stream.bounded_push)
    (sender : Cstruct.t -> unit Lwt.t)
    (req : a Request.t) =
  match React.S.value state with
  | `Init | `No_response -> Lwt.return_error Request.Not_responding
  | `Fine ->
    Lwt.catch (fun () ->
        let t, w = Lwt.task () in
        let send = fun stream ->
          Fsm.request src stream sender req
          >>= fun x -> Lwt.wakeup_later w x; Lwt.return_unit in
        Lwt.pick [await_no_response state; (push#push send >>= fun () -> t)])
      (function
        | Lwt.Canceled -> Lwt.return_error Request.Not_responding
        | Lwt_stream.Full -> Lwt.return_error Request.Queue_overflow
        | exn -> Lwt.fail exn)

let create
    (src : Logs.src)
    (sender : Cstruct.t -> unit Lwt.t)
    (streams_conv : Stream.Raw.t list React.signal -> Stream.t list React.signal)
    (kv : config Kv_v.rw) =
  let state, set_state =
    React.S.create ~eq:Topology.equal_state `No_response in
  let devinfo, set_devinfo =
    React.S.create ~eq:(Boards.Util.Option.equal equal_devinfo) None in
  let notifs =
    { state
    ; devinfo
    } in
  let req_queue, push_req_queue = Lwt_stream.create_bounded msg_queue_size in
  let rsp_queue, push_rsp_queue = Lwt_stream.create () in
  let push_data =
    let acc = ref None in
    let parts = ref [] in
    let push (buf : Cstruct.t) =
      let buf = match !acc with
        | None -> buf
        | Some acc -> Cstruct.append acc buf in
      let parsed, new_parts, new_acc = Parser.deserialize src !parts buf in
      acc := new_acc;
      parts := new_parts;
      List.iter (fun x -> push_rsp_queue @@ Some (`Simple x)) parsed in
    push in
  let channel = fun req -> send src state push_req_queue sender req in
  let loop () =
    Fsm.start src sender req_queue rsp_queue kv
      set_state
      (fun x -> set_devinfo @@ Some x) in
  Lwt.return_ok { notifs
                ; kv
                ; channel
                ; loop
                ; push_data
                }
