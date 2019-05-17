open Board_niitv_tsan_types
open Application_types

type notifs =
  { state : Topology.state React.signal
  ; devinfo : devinfo option React.signal
  ; status : Parser.Status.t React.event
  ; streams : Stream.t list React.signal
  ; bitrate : (Stream.ID.t * Bitrate.t) list React.event
  ; structure : (Stream.ID.t * Structure.t) list React.signal
  ; t2mi_info : (Stream.ID.t * (int * T2mi_info.t) list) list React.signal
  ; deverr : Deverr.t list React.event
  ; errors : (Stream.ID.t * Error.t list) list React.event
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
  match React.S.value state with
  | `No_response | `Init -> Lwt.return_error Request.Not_responding
  | `Fine ->
    Util_react.(
      E.next
      @@ E.fmap (function
          | `Init | `No_response -> Some (Error Request.Not_responding)
          | `Fine -> None)
      @@ S.changes state)

let send (type a)
    (src : Logs.src)
    (state : Topology.state React.signal)
    (push : Fsm.api_msg -> unit Lwt.t)
    (sender : Fsm.sender)
    (req : a Request.t) =
  match React.S.value state with
  | `Init | `No_response -> Lwt.return_error Request.Not_responding
  | `Fine ->
    Lwt.catch (fun () ->
        let t, w = Lwt.task () in
        let send = fun stream events ->
          Fsm.request src stream events sender req
          >>= fun x -> Lwt.wakeup_later w x; Lwt.return_unit in
        Lwt.pick
          [ await_no_response state
          ; (push ((Request.to_enum req), send) >>= fun () -> t)
          ])
      (function
        | Lwt.Canceled -> Lwt.return_error Request.Not_responding
        | Queue_lwt.Full -> Lwt.return_error Request.Queue_overflow
        | exn -> Lwt.fail exn)

let rec map_response prev ({ data; timestamp } as rsp : Request.rsp ts) =
  match data with
  | `Complex _ -> `R rsp
  | `Simple ({ tag = `Status; _ } as data) -> `E { data; timestamp }
  | `Simple ({ tag = `Streams; _ } as data) -> `E { data; timestamp }
  | `Simple ({ tag = `Ts_errors; _ } as data) -> `E { data; timestamp }
  | `Simple ({ tag = `T2mi_errors; _ } as data) -> `E { data; timestamp }
  | `Simple ({ tag = `End_of_errors; _ } as data) -> `E { data; timestamp }
  | `Simple ({ tag = `End_of_transmission; _ } as data) ->
    (match prev with
     | None -> `R rsp
     | Some prev ->
       (match map_response None prev with
        | `R _ -> `R rsp
        | `E _ -> `E { data; timestamp }))
  | r -> `R rsp

let map_stream_id
    (streams : Stream.t list React.signal)
    (event : (Stream.Multi_TS_ID.t * 'a) list React.event) =
  Util_react.S.sample (fun event streams ->
      Boards.Util.List.filter_map (fun (id, v) ->
          match Stream.find_by_multi_id id streams with
          | None -> None
          | Some s -> Some (s.id, v)) event)
    event streams

let map_errors
    (errors : ('a * Error.t list) list)
    (structures : ('a * Structure.t) list)
  : ('a * Error.t_ext list) list =
  List.map (fun (id, errors) ->
      match List.assoc_opt id structures with
      | None -> id, List.map (fun (e : Error.t) ->
          { e with pid = (e.pid, None) }) errors
      | Some { pids; _ } ->
        let errors =
          List.map (fun (e : Error.t) ->
              { e with pid = (e.pid, List.assoc_opt e.pid pids) }) errors in
        id, errors) errors

let change_t2mi_mode
    (streams : Stream.t list React.signal)
    (config : config React.signal)
  : t2mi_mode Lwt_stream.t =
  React.S.sample (fun streams ({ t2mi_mode = mode; _ } : config) ->
      match mode.stream_id with
      | None -> None
      | Some id ->
        match Stream.find_by_id id streams with
        | None -> Some { mode with enabled = false }
        | Some { orig_id = TS_multi stream; _ } -> Some { mode with stream }
        | Some _ -> None)
    (React.S.changes streams) config
  |> React.E.fmap (fun x -> x)
  |> React.E.changes ~eq:equal_t2mi_mode
  |> Util_react.E.to_stream

let create
    (src : Logs.src)
    (sender : Cstruct.t -> unit Lwt.t)
    (streams_conv : Stream.Raw.t list React.signal -> Stream.t list React.signal)
    (kv : config Kv_v.rw) =
  let state, set_state =
    React.S.create ~eq:Topology.equal_state `No_response in
  let devinfo, set_devinfo =
    React.S.create ~eq:(Util_equal.Option.equal equal_devinfo) None in
  let status, set_status = React.E.create () in
  let errors, set_errors = React.E.create () in
  let raw_streams, set_raw_streams = React.S.create [] in
  let structure, set_structure = React.E.create () in
  let bitrate, set_bitrate = React.E.create () in
  let t2mi_info, set_t2mi_info = React.E.create () in
  let deverr, set_deverr = React.E.create () in
  let streams = streams_conv raw_streams in
  let t2mi_mode_listener = change_t2mi_mode streams kv#s in
  let notifs =
    { state
    ; devinfo
    ; deverr
    ; status
    ; streams
    ; bitrate = map_stream_id streams bitrate
    ; errors = map_stream_id streams errors
    ; t2mi_info = Util_equal.(
          let eq_t2mi = List.equal @@ Pair.equal Int.equal T2mi_info.equal in
          let eq = List.equal @@ Pair.equal Stream.ID.equal eq_t2mi in
          React.S.hold ~eq [] @@ map_stream_id streams t2mi_info)
    ; structure = Util_equal.(
          let eq = List.equal @@ Pair.equal Stream.ID.equal Structure.equal in
          React.S.hold ~eq [] @@ map_stream_id streams structure)
    } in
  let sender = { Fsm. send = fun req -> sender @@ Serializer.serialize req } in
  let (pending : Fsm.pending ref) = ref [] in
  let pred = (fun (id, _) -> not @@ List.mem_assoc id !pending) in
  let req_queue, push_req_queue = Queue_lwt.create msg_queue_size pred in
  let rsp_event, push_rsp_event = React.E.create () in
  let evt_queue, push_evt_queue = Lwt_stream.create () in
  let prev = ref None in
  let acc = ref None in
  let parts = ref Parser.Part.empty in
  (* Complex message part TTL (in seconds). *)
  let parts_ttl = 5. in
  let push_data (buf : Cstruct.t) =
    let buf = match !acc with
      | None -> buf
      | Some acc -> Cstruct.append acc buf in
    let timestamp = Ptime_clock.now () in
    let parts' = Parser.Part.filter (fun _ (acc : Parser.part list ts) ->
        let old = Ptime.to_float_s acc.timestamp in
        let cur = Ptime.to_float_s timestamp in
        cur -. old < parts_ttl) !parts in
    let parsed, new_parts, new_acc = Parser.deserialize ~timestamp src parts' buf in
    acc := new_acc;
    parts := new_parts;
    List.iter (fun (x : Request.rsp ts) ->
        (* Logs.debug ~src (fun m ->
         *     m "Got '%s'" (match x.data with
         *         | `Complex { tag; _ } -> Request.complex_tag_to_string tag
         *         | `Simple { tag; _ } -> Request.simple_tag_to_string tag)); *)
        (match map_response !prev x with
         | `R r -> push_rsp_event r
         | `E e -> push_evt_queue @@ Some e);
        prev := Some x) parsed in
  let channel = fun req -> send src state push_req_queue sender req in
  let loop () =
    Fsm.start src sender pending req_queue rsp_event evt_queue kv
      t2mi_mode_listener
      set_state
      (fun ?step x -> set_devinfo ?step @@ Some x)
      set_status
      set_errors
      set_raw_streams
      set_structure
      set_bitrate
      set_t2mi_info
      set_deverr in
  Lwt.return_ok { notifs
                ; kv
                ; channel
                ; loop
                ; push_data
                }
