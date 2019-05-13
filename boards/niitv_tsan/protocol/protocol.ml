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

let create (src : Logs.src)
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
  (* let req_queue, push_req_queue = Lwt_stream.create_bounded msg_queue_size in *)
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
      List.iter (fun x -> push_rsp_queue @@ Some x) parsed in
    push in
  let channel _ = assert false in
  let loop () = Lwt.return_unit in
  Lwt.return_ok { notifs
                ; kv
                ; channel
                ; loop
                ; push_data
                }
