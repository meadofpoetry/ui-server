open Application_types
open Board_dektec_dtm3200_types
open Util_react

let ( >>= ) = Lwt.bind

let msg_queue_size = 20

type notifs =
  { streams : Stream.t list React.signal
  ; devinfo : devinfo option React.signal
  ; state : Topology.state React.signal
  ; status : status React.event
  ; config : config React.signal
  }

type api =
  { kv : config Kv_v.rw
  ; notifs : notifs
  ; channel : 'a. 'a Request.t -> ('a, Request.error) Lwt_result.t
  ; loop : unit -> unit Lwt.t
  ; push_data : Cstruct.t -> unit
  }

let send (type a) ~(address : int)
    (src : Logs.src)
    (state : Topology.state React.signal)
    (push : _ Lwt_stream.bounded_push)
    (sender : Cstruct.t -> unit Lwt.t)
    (kv : config Kv_v.rw)
    (req : a Request.t) =
  match React.S.value state with
  | `Init | `No_response -> Lwt.return_error Request.Not_responding
  | `Fine ->
    Lwt.catch (fun () ->
        let t, w = Lwt.task () in
        let stop = fun error -> Lwt.wakeup_later w (Error error) in
        let send = fun stream ->
          Fsm_common.request src sender stream kv req
          >>= fun x -> Lwt.wakeup_later w x; Lwt.return_unit in
        push#push @@ (send, stop) >>= fun () -> t)
      (function
        | Lwt.Canceled -> Lwt.return_error Request.Not_responding
        | Lwt_stream.Full -> Lwt.return_error Request.Queue_overflow
        | exn -> Lwt.fail exn)

let to_streams_s (config : config signal) (status : status event) =
  S.hold ~eq:(Util_equal.List.equal Stream.Raw.equal) []
  @@ S.sample (fun ({ asi_bitrate; protocol; _ } : status)
                ({ ip; nw } : config) ->
                if asi_bitrate <= 0 then [] else (
                  let scheme = match protocol with
                    | RTP -> "rtp"
                    | UDP -> "udp" in
                  let (info : Stream.Source.ipv4) = match ip.multicast with
                    | Some x ->
                      { addr = x
                      ; port = ip.port
                      ; scheme
                      }
                    | None ->
                      { addr = nw.ip
                      ; port = ip.port
                      ; scheme
                      } in
                  let (stream : Stream.Raw.t) =
                    { source = { info = IPV4 info; node = Port 0 }
                    ; id = TS_raw
                    ; typ = TS
                    } in
                  [stream]))
    status config

let create (src : Logs.src)
    (sender : Cstruct.t -> unit Lwt.t)
    streams_conv
    (kv : config Kv_v.rw)
    (_ : Db.t) =
  let status, set_status = E.create () in
  let state, set_state = S.create ~eq:Topology.equal_state `No_response in
  let devinfo, set_devinfo =
    S.create ~eq:(Util_equal.Option.equal equal_devinfo) None in
  let notifs =
    { streams = streams_conv @@ to_streams_s kv#s status
    ; devinfo
    ; state
    ; status
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
      let address = (React.S.value kv#s).address in
      let parsed, new_acc = Parser.deserialize ~address src buf in
      acc := new_acc;
      List.iter (fun x -> push_rsp_queue @@ Some x) parsed in
    push in
  let channel = fun req ->
    kv#get
    >>= fun { address; _ } ->
    send ~address src state push_req_queue sender kv req in
  let loop =
    Fsm.start src sender req_queue rsp_queue kv
      set_state
      (fun x -> set_devinfo @@ Some x)
      set_status in
  let (api : api) =
    { notifs
    ; loop
    ; push_data
    ; kv
    ; channel
    } in
  Lwt.return_ok api
