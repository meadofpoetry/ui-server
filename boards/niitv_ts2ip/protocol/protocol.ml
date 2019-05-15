open Board_niitv_ts2ip_types
open Application_types

(* TODO remove after 4.08 *)
module List = Boards.Util.List

type notifs =
  { state : Topology.state React.signal
  ; device_status : device_status React.event
  ; transmitter_status : transmitter_status React.event
  ; devinfo : devinfo option React.signal
  ; config : config React.signal
  ; incoming_streams : Stream.t list React.signal
  ; outgoing_streams : Stream.t list React.signal
  }

type api =
  { notifs : notifs
  ; kv : config Kv_v.rw
  ; ports : Topology.topo_port list
  ; channel : 'a. 'a Request.t -> ('a, Request.error) Lwt_result.t
  ; loop : unit -> unit Lwt.t
  ; push_data : Cstruct.t -> unit
  }

let msg_queue_size = 20

let ( % ) f g x = f (g x)

let ( >>= ) = Lwt.Infix.( >>= )

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

let find_stream (ports : Topology.topo_port list)
    (stream : Stream.t)
    (socket : socket)
    (streams : Stream.t list) =
  List.find_opt (fun (t : Stream.t) ->
      let p = Stream.to_topo_port ports t in
      let port = socket_to_enum socket in
      match p with
      | Some p when Stream.equal t stream && p.port = port -> true
      | _ -> false) streams

(* let to_out_streams_s (ports : Topology.topo_port list)
 *     (status : status React.event)
 *     (streams : Stream.t list React.signal) =
 *   let status =
 *     let eq =
 *       Equal.pair equal_packer_settings equal_packer_status
 *       |> Equal.list in
 *     React.E.map (fun x -> x.packers_status) status
 *     |> React.S.hold ~eq [] in
 *   React.S.l2 ~eq:(List.equal Stream.equal) (fun status streams ->
 *       List.fold_left (fun acc ((mode : udp_mode),
 *                                { bitrate; enabled; has_data; _ }) ->
 *                        let s = find_stream ports mode.stream mode.socket streams in
 *                        match s, bitrate, enabled, has_data with
 *                        | Some s, Some _, true, true ->
 *                          let (stream : Stream.t) =
 *                            { source = s.source
 *                            ; orig_id = TSoIP { addr = mode.dst_ip
 *                                              ; port = mode.dst_port }
 *                            ; id = s.id
 *                            ; typ = s.typ
 *                            }
 *                          in stream :: acc
 *                        | _ -> acc) [] status)
 *     status streams *)

let port_to_stream (port : Topology.topo_port) =
  match port.child with
  | Board _ -> None
  | Input _ ->
    match socket_of_enum port.port with
    | None -> None
    | Some socket ->
      let info = match socket with
        | ASI_1 | ASI_2 -> Stream.Source.ASI
        | SPI_1 | SPI_2 | SPI_3 -> SPI in
      let id = Stream.Multi_TS_ID.make ~source_id:0 ~stream_id:0 in
      Some (socket, { Stream.Raw.
                      source = { info; node = Port port.port }
                    ; id = TS_multi id
                    ; typ = TS
                    })

let update_incoming_streams
    (streams : Stream.t list React.signal)
    (conv : Stream.Raw.t list React.signal -> Stream.t list React.signal)
    (status : device_status React.event)
    (ports : Topology.topo_port list) =
  let inputs = List.filter_map port_to_stream ports in
  let input_streams =
    React.S.hold []
    @@ React.E.fmap (fun (sync : socket list) ->
        match List.filter_map (fun id -> List.assoc_opt id inputs) sync with
        | [] -> None
        | l -> Some l)
    @@ React.E.changes ~eq:(Util_equal.List.equal equal_socket)
    @@ React.E.map (fun (status : device_status) -> status.sync) status in
  React.S.merge ~eq:(Util_equal.List.equal Stream.equal) (@)
    [] [conv input_streams; streams]

let create (src : Logs.src)
    (sender : Cstruct.t -> unit Lwt.t)
    (incoming_streams : Stream.t list React.signal)
    (streams_conv : Stream.Raw.t list React.signal -> Stream.t list React.signal)
    (kv : config Kv_v.rw)
    (ports : Topology.topo_port list)
    (control : int) =
  let state, set_state =
    React.S.create ~eq:Topology.equal_state `No_response in
  let devinfo, set_devinfo =
    React.S.create ~eq:(Util_equal.Option.equal equal_devinfo) None in
  let device_status, set_device_status = React.E.create () in
  let transmitter_status, set_transmitter_status = React.E.create () in
  let incoming_streams =
    update_incoming_streams
      incoming_streams
      streams_conv
      device_status
      ports in
  (* let out_streams = to_out_streams_s board status streams in *)
  let outgoing_streams = React.S.const [] in
  let (notifs : notifs) =
    { state
    ; devinfo
    ; incoming_streams
    ; outgoing_streams
    ; device_status
    ; transmitter_status
    ; config = kv#s
    } in
  let req_queue, push_req_queue = Lwt_stream.create_bounded msg_queue_size in
  let rsp_queue, push_rsp_queue = Lwt_stream.create () in
  let push_data =
    let acc = ref None in
    let push (buf : Cstruct.t) =
      let buf = match !acc with
        | None -> buf
        | Some acc -> Cstruct.append acc buf in
      let parsed, new_acc = Parser.deserialize src buf in
      acc := new_acc;
      List.iter (fun x -> push_rsp_queue @@ Some x) parsed in
    push in
  let channel = fun req -> send src state push_req_queue sender req in
  let loop = Fsm.start src sender req_queue rsp_queue kv
      set_state
      (fun x -> set_devinfo @@ Some x)
      set_device_status
      set_transmitter_status in
  let api =
    { notifs
    ; loop
    ; ports
    ; push_data
    ; channel
    ; kv
    } in
  Lwt.return_ok api
