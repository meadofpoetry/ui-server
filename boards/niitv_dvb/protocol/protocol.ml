open Board_niitv_dvb_types
open Application_types

let ( >>= ) = Lwt.bind

type notifs =
  { devinfo : Device.info option React.signal
  ; state : Topology.state React.signal
  ; config : Device.config React.signal
  ; measures : (int * Measure.t ts) list React.event
  ; params : (int * Params.t ts) list React.event
  ; plps : (int * Plp_list.t ts) list React.event
  ; raw_streams : Stream.Raw.t list React.signal
  ; streams : Stream.t list React.signal
  }

type api =
  { kv : Device.config Kv_v.rw
  ; notifs : notifs
  ; channel : 'a. 'a Request.t -> ('a, Request.error) Lwt_result.t
  ; loop : unit -> unit Lwt.t
  ; push_data : Cstruct.t -> unit
  ; model : Model.t
  }

let msg_queue_size = 20

let send (type a) (src : Logs.src)
    (state : Topology.state React.signal)
    (push : _ Lwt_stream.bounded_push)
    (sender : Cstruct.t -> unit Lwt.t)
    (req : a Request.t) =
  match React.S.value state with
  | `Init | `No_response | `Detect -> Lwt.return_error Request.Not_responding
  | `Fine ->
    Lwt.catch (fun () ->
        let t, w = Lwt.task () in
        let send = fun stream ->
          Fsm.request src stream sender req
          >>= fun x -> Lwt.wakeup_later w x; Lwt.return_unit in
        Lwt.pick
          [ (Boards.Board.await_no_response state >>= Api_util.not_responding)
          ; (push#push send >>= fun () -> t)
          ])
      (function
        | Lwt.Canceled -> Lwt.return_error Request.Not_responding
        | Lwt_stream.Full -> Lwt.return_error Request.Queue_overflow
        | exn -> Lwt.fail exn)

(** Converts tuner mode to raw stream. *)
let mode_to_stream (source_id : int)
    (id, ({ standard; channel } : Device.mode)) : Stream.Raw.t =
  let open Stream in
  let (freq : int64) = Int64.of_int channel.freq in
  let (bw : float) = match channel.bw with
    | Bw8 -> 8.
    | Bw7 -> 7.
    | Bw6 -> 6. in
  let (info : Source.t) = match standard with
    | C -> DVB_C { freq; bw}
    | T -> DVB_T { freq; bw }
    | T2 -> DVB_T2 { freq; bw; plp = channel.plp } in
  let id = Multi_TS_ID.make ~source_id ~stream_id:id in
  { source = { info; node = Port 0 }
  ; id = TS_multi id
  ; typ = TS
  }

(** Converts device config signal to raw stream list signal. *)
let to_streams_s (config : Device.config React.signal)
  : Stream.Raw.t list React.signal =
  React.S.map ~eq:(Util_equal.List.equal Stream.Raw.equal)
    (fun (x : Device.config) -> List.map (mode_to_stream x.source) x.mode)
    config

(** Converts absolute measured channel frequency value
    to frequency offset in measurements event. *)
let map_measures (config : Device.config React.signal)
    (e : (int * Measure.t ts) list React.event)
  : (int * Measure.t ts) list React.event =
  React.S.sample (fun l (config : Device.config) ->
      Boards.Util.List.filter_map
        (fun (id, ({ data; timestamp } : Measure.t ts)) ->
           match List.assoc_opt id config.mode with
           | None -> None
           | Some (mode : Device.mode) ->
             let freq = match data.freq with
               | None -> None
               | Some x -> Some (mode.channel.freq - x) in
             let (data : Measure.t ts) =
               { data = { data with freq }; timestamp } in
             Some (id, data)) l) e config
  |> React.E.fmap (function [] -> None | l -> Some l)

let create (src : Logs.src)
    (sender : Cstruct.t -> unit Lwt.t)
    streams_conv
    (kv : Device.config Kv_v.rw)
    (control : int)
    (db : Db.t) =
  let open Util_react in
  let devinfo, set_devinfo =
    S.create ~eq:(Util_equal.Option.equal Device.equal_info) None in
  let measures, set_measures = E.create () in
  let params, set_params = E.create () in
  let plps, set_plps = E.create () in
  let state, set_state = S.create ~eq:Topology.equal_state `No_response in
  let raw_streams = to_streams_s kv#s in
  let streams = streams_conv raw_streams in
  let (notifs : notifs) =
    { measures = map_measures kv#s measures
    ; params
    ; plps
    ; devinfo
    ; config = kv#s
    ; state
    ; raw_streams
    ; streams
    } in
  Lwt_result.Infix.(
    Model.create src control measures db
    >>= fun model ->
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
    let loop =
      Fsm.start src sender req_queue rsp_queue kv
        set_state
        (fun x -> set_devinfo @@ Some x)
        set_measures
        set_params
        set_plps in
    let api =
      { notifs
      ; loop
      ; push_data
      ; channel
      ; model
      ; kv
      } in
    Lwt.return_ok api)
