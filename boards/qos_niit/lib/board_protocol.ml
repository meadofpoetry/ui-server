open Containers
open Board_qos_types
open Storage.Options
open Boards
open Boards.Pools
open Board_parser
open Common

let status_timeout = 8. (* seconds *)
let detect_timeout = 3. (* seconds *)
let probes_timeout = 3. (* seconds *)

module Timer = Boards.Timer

(** Checks if the stream is received from board input *)
let is_incoming ({ t2mi; _ } : init) =
  fun (s : Stream.t) ->
  match s.orig_id with
  | TS_multi x ->
     let source_id = Multi_TS_ID.source_id x in
     source_id <> t2mi
  | _ -> false

let t2mi_mode_to_raw (mode : t2mi_mode option) =
  match mode with
  | None -> t2mi_mode_raw_default
  | Some { stream; pid; enabled; t2mi_stream_id } ->
     begin match stream.orig_id with
     | TS_multi x -> { pid; enabled; t2mi_stream_id; stream = x }
     | _ -> t2mi_mode_raw_default (* XXX maybe throw an exn here? *)
     end

module Make(Logs : Logs.LOG) : sig
  val create : init ->
               (Cstruct.t -> unit Lwt.t) ->
               config storage ->
               float ->
               (Stream.Raw.t list React.signal -> Stream.t list React.signal) ->
               events * api * ([`Continue of Cstruct.t list -> 'a ] as 'a)
end = struct

  type protocol_error =
    | Unexpected_info of devinfo
    | Unexpected_status_idle
    | Unexpected_status_probes
    | Status_timeout of float
    | Probes_timeout of float

  let protocol_error_to_int = function
    | Unexpected_info _ -> 0
    | Unexpected_status_idle -> 1
    | Unexpected_status_probes -> 2
    | Status_timeout _ -> 3
    | Probes_timeout _ -> 4

  let board_error_of_protocol_error (e : protocol_error) : Board_error.t =
    { time = Ptime_clock.now ()
    ; code = protocol_error_to_int e
    ; source = Protocol
    ; count = 1
    ; param = None
    }

  module Serializer = Board_serializer.Make(Logs)
  module Parser = Board_parser.Make(Logs)
  module Events = Board_event_handler.Make(Logs)
  module Acc = Board_event_handler.Acc

  exception Protocol_invariant of protocol_error

  let wakeup_timeout (_, t) =
    t.pred `Timeout |> ignore

  let check_board_info rsps =
    match List.find_map (Parser.is_response Get_board_info) rsps with
    | None -> ()
    | Some x -> raise_notrace (Protocol_invariant (Unexpected_info x))

  let push_state (pe : push_events) state =
    pe.bitrates [];
    pe.state state

  let step sources msgs imsgs sender (storage : config storage) step_duration
        (events : events) (pe : push_events) =

    let board_info_err_msg =
      "board info was received during normal \
       operation, restarting..." in

    let no_status_msg period =
      Printf.sprintf "no status received for %g seconds, restarting..."
        period in

    let deserialize ~with_init_exn (acc : Acc.t) recvd =
      let events, probes, rsps, parts, bytes =
        Parser.deserialize acc.parts (Board.concat_acc acc.bytes recvd) in
      (* raises exception if board info message is found *)
      if with_init_exn then check_board_info rsps;
      let events = acc.events @ events in
      probes, rsps, { acc with events; parts; bytes } in

    let handle_msgs rsps =
      if Await_queue.has_pending !msgs
      then (msgs := fst @@ Await_queue.responsed !msgs rsps;
            let new_msgs, tout = Await_queue.step !msgs in
            msgs := new_msgs;
            (match tout with
             | [] -> ()
             | l -> List.iter (fun x -> x.pred `Timeout |> ignore) l));
      if not @@ Await_queue.empty !msgs
      then msgs := fst @@ Await_queue.send !msgs () in

    let rec first_step () =
      Logs.info (fun m -> m "start of connection establishment...");
      Await_queue.iter !msgs wakeup_timeout;
      msgs := Await_queue.create [];
      imsgs := Queue.create [];
      push_state pe `No_response;
      Serializer.send_msg sender Get_board_info |> Lwt.ignore_result;
      `Continue (step_detect (Timer.create ~step_duration detect_timeout) Acc.empty)

    and step_detect (timer : Timer.t) (acc : Acc.t) recvd =
      try
        let _, rsps, acc = deserialize ~with_init_exn:false acc recvd in
        match List.find_map (Parser.is_response Get_board_info) rsps with
        | None -> `Continue (step_detect (Timer.step timer) acc)
        | Some info -> `Continue (step_init info acc)
      with Timer.Timeout t ->
        (Logs.warn (fun m ->
             m "connection is not established after %g seconds, \
                restarting" (Timer.period t));
         first_step ())

    and step_init devinfo (acc : Acc.t) _ =
      push_state pe `Init;
      pe.devinfo (Some devinfo);
      let { t2mi_mode; input; jitter_mode } : config = storage#get in
      let t2mi_mode_raw = t2mi_mode_to_raw t2mi_mode in
      Serializer.send_instant sender (Set_board_init sources)
      |> Lwt.ignore_result;
      Serializer.send_instant sender (Set_board_mode (input, t2mi_mode_raw))
      |> Lwt.ignore_result;
      Serializer.send_instant sender (Set_jitter_mode jitter_mode)
      |> Lwt.ignore_result;
      Logs.info (fun m -> m "connection established, waiting \
                             for 'status' message");
      let exn = Protocol_invariant (Status_timeout status_timeout) in
      let timer = Timer.create ~exn ~step_duration status_timeout in
      `Continue (step_ok_idle timer acc)

    and step_ok_idle (timer : Timer.t) (acc : Acc.t) recvd =
      try
        let _, rsps, acc = deserialize ~with_init_exn:true acc recvd in
        handle_msgs rsps;
        if not (Queue.empty !imsgs)
        then Queue.send !imsgs () |> Lwt.ignore_result;
        imsgs := Queue.next !imsgs;
        begin match Events.partition acc with
        | [], acc -> `Continue (step_ok_idle (Timer.step timer) acc)
        | [group], acc ->
           if Option.is_none acc.group
           then (Logs.info (fun m -> m "initialization done!");
                 push_state pe `Fine);
           Logs.debug (fun m ->
               m "received group: load=%g%%" group.status.basic.load);
           let stack = Events.get_req_stack group acc.group in
           let exn = Protocol_invariant (Probes_timeout probes_timeout) in
           let pool =
             List.map (fun req ->
                 { send = (fun () -> Serializer.send_event sender req)
                 ; pred = Parser.is_probe_response req
                 ; timeout = Timer.steps ~step_duration probes_timeout
                 ; exn = Some exn }) stack
             |> Pool.create in
           Logs.debug (fun m ->
               let pre = "prepared stack of probe requests" in
               let stk =
                 String.concat "; "
                 @@ List.map Serializer.probe_req_to_string stack in
               m "%s: [%s]" pre stk);
           let acc = { acc with group = Some group } in
           let acc = Events.handle_immediate pe acc in
           step_ok_probes_send pool (Timer.reset timer) acc
        | _ -> raise_notrace (Protocol_invariant Unexpected_status_idle)
        end
      with Protocol_invariant e -> handle_invariant e

    and step_ok_probes_send pool (timer : Timer.t) (acc : Acc.t) =
      if Pool.empty pool
      then
        let acc = Events.handle pe events sources acc in
        `Continue (step_ok_idle (Timer.reset timer) acc)
      else (Pool.send pool () |> Lwt.ignore_result;
            `Continue (step_ok_probes_wait pool timer acc))

    and step_ok_probes_wait pool (timer : Timer.t) (acc : Acc.t) recvd =
      try
        let probes, rsps, acc = deserialize ~with_init_exn:true acc recvd in
        handle_msgs rsps;
        if not (Queue.empty !imsgs)
        then Queue.send !imsgs () |> Lwt.ignore_result;
        imsgs := Queue.next !imsgs;
        let timer, acc = match Events.partition acc with
          | [], acc -> (Timer.step timer), acc
          | _ -> raise_notrace (Protocol_invariant Unexpected_status_probes) in
        begin match Pool.responsed pool probes with
        | None -> `Continue (step_ok_probes_wait (Pool.step pool) timer acc)
        | Some x ->
           Logs.debug (fun m ->
               m "got probe response: '%s'"
               @@ Serializer.probe_rsp_to_string x);
           let acc = Acc.cons_probe acc x in
           begin match Pool.last pool with
           | true ->
              let (acc : Acc.t) =
                Events.handle pe events sources acc in
              `Continue (step_ok_idle timer acc)
           | false -> step_ok_probes_send (Pool.next pool) timer acc
           end
        end
      with Protocol_invariant e -> handle_invariant e

    and handle_invariant (e : protocol_error) =
      let error = board_error_of_protocol_error e in
      pe.board_errors [error];
      begin match e with
      | Unexpected_info info ->
         Logs.warn (fun m -> m "%s" board_info_err_msg);
         push_state pe `No_response;
         `Continue (step_init info Acc.empty)
      | Unexpected_status_idle ->
         Logs.warn (fun m ->
             m "Got more than one status at once, restarting...");
         first_step ()
      | Unexpected_status_probes ->
         Logs.warn (fun m -> m "got status while waiting for probes. \
                                Looks like an overload, restarting...");
         first_step ()
      | Status_timeout period ->
         Logs.warn (fun m -> m "%s" (no_status_msg period));
         first_step ()
      | Probes_timeout period ->
         Logs.warn (fun m ->
             m "No probe response for %g sec, restarting..." period);
         first_step ()
      end

    in
    first_step ()

  open React

  (* T2-MI mode signal, contains value to apply to the board *)
  (* FIXME check this *)
  (* let to_t2mi_locker
   *       (mode : t2mi_mode option signal)
   *       (streams : Stream.t list signal) =
   *   let find (streams : Stream.t list) = function
   *     | None -> None
   *     | Some (stream : Stream.t) ->
   *        List.find_opt (fun (s : Stream.t) ->
   *            not @@ Stream.ID.equal s.id stream.id
   *            && Stream.equal_container_id s.orig_id stream.orig_id) streams in
   *   React.S.l2 ~eq:Equal.bool (fun streams (mode : t2mi_mode option) ->
   *       let stream = Option.map (fun (x : t2mi_mode) -> x.stream) mode in
   *       match find streams stream with
   *       | None -> false
   *       | Some _ -> true) streams mode *)

  (* Map SI/PSI tables to separate SI/PSI sections *)
  let to_sections (tables : SI_PSI_table.t list) : SI_PSI_section.t list =
    let open SI_PSI_section in
    List.flat_map (fun ((id : SI_PSI_table.id), (info : SI_PSI_table.info)) ->
        List.map (fun ({ section; length } : SI_PSI_table.section_info) ->
            let (id : id) =
              { table_id = id.table_id
              ; table_id_ext = id.table_id_ext
              ; id_ext_1 = id.id_ext_1
              ; id_ext_2 = id.id_ext_2
              ; section
              } in
            let (info : info) =
              { pid = info.pid
              ; version = info.version
              ; service_id = info.service_id
              ; service_name = info.service_name
              ; section_syntax = info.section_syntax
              ; last_section = info.last_section
              ; eit_segment_lsn = info.eit_segment_lsn
              ; eit_last_table_id = info.eit_last_table_id
              ; length
              } in
            id, info) info.sections) tables

  let create_events (storage : config storage) streams_conv =
    let state, set_state =
      S.create ~eq:Topology.equal_state `No_response in
    let input, set_input =
      S.create ~eq:equal_input storage#get.input in
    let devinfo, set_devinfo =
      S.create ~eq:(Equal.option equal_devinfo) None in
    let status, set_status =
      S.create ~eq:equal_status
        { time = Time.epoch
        ; load = 0.0
        ; reset = false
        ; ts_num = 0
        ; services_num = 0
        ; bitrate = 0
        ; packet_sz = Ts188
        ; has_sync = false
        ; has_stream = false
        } in
    let hw_errors, set_hw_errors = E.create () in
    let raw_streams, set_raw_streams =
      S.create ~eq:(Equal.list Stream.Raw.equal)[] in
    let structures, set_structures =
      S.create ~eq:(Equal.list (Equal.pair Multi_TS_ID.equal equal_structure)) [] in
    let ts_info, set_ts_info =
      S.create ~eq:equal_ts_info [] in
    let services, set_services =
      S.create ~eq:equal_services [] in
    let tables, set_tables =
      S.create ~eq:equal_tables [] in
    let pids, set_pids =
      S.create ~eq:equal_pids [] in
    let bitrates, set_bitrates = E.create () in
    let t2mi_info, set_t2mi_info =
      S.create ~eq:equal_t2mi_info [] in
    let ts_errors, set_ts_errors = E.create () in
    let t2mi_errors, set_t2mi_errors = E.create () in
    let t2mi_mode_raw, set_t2mi_mode_raw = E.create () in
    let t2mi_mode, set_t2mi_mode =
      S.create ~eq:(Equal.option equal_t2mi_mode) storage#get.t2mi_mode in
    let jitter_mode, set_jitter_mode =
      S.create ~eq:(Equal.option equal_jitter_mode) storage#get.jitter_mode in
    let config =
      S.l3 ~eq:equal_config (fun input t2mi_mode jitter_mode ->
          let config = { input; t2mi_mode; jitter_mode } in
          storage#store config;
          config)
        input t2mi_mode jitter_mode in
    let e_pcr, pcr_push = E.create () in
    let e_pcr_s, pcr_s_push = E.create () in

    let streams = streams_conv raw_streams in
    let sections =
      S.map ~eq:equal_sections
        (List.map (fun (id, { timestamp; data }) ->
             id, { timestamp; data = to_sections data })) tables in
    let device =
      { state; input; info = devinfo; t2mi_mode; jitter_mode; config;
        status; errors = hw_errors } in
    let ts =
      { info = ts_info; services; sections; tables; pids;
        bitrates; errors = ts_errors } in
    let t2mi =
      { structures = t2mi_info; errors = t2mi_errors } in
    let jitter =
      { session = E.changes ~eq:Jitter.equal_session e_pcr_s
      ; jitter = e_pcr } in
    let raw =
      { structures; t2mi_mode_raw } in
    let (events : events) =
      { device; ts; t2mi; raw; streams; jitter } in
    let (push_events : push_events) =
      { devinfo = set_devinfo; input = set_input; status = set_status;
        t2mi_mode_raw = set_t2mi_mode_raw; t2mi_mode = set_t2mi_mode;
        jitter_mode = set_jitter_mode; state = set_state;
        raw_streams = set_raw_streams; ts_errors = set_ts_errors;
        t2mi_errors = set_t2mi_errors; board_errors = set_hw_errors;
        info = set_ts_info; services = set_services; tables = set_tables;
        pids = set_pids; bitrates = set_bitrates; t2mi_info = set_t2mi_info;
        structures = set_structures; jitter = pcr_push;
        jitter_session = pcr_s_push } in
    events, push_events

  let wait = fun ~eq cur req event ->
    Lwt.(req ()
         >>= fun () ->
         Lwt.pick [ E.next (E.filter (eq cur) event)
                  ; Lwt_unix.timeout status_timeout ])

  let create_api sources sender step_duration
        (storage : config storage) events (push_events : push_events) =
    let open Lwt.Infix in
    let { device = { config; state; _ }; _ }  = events in
    let msgs = ref (Await_queue.create []) in
    let imsgs = ref (Queue.create []) in
    let isend = Serializer.enqueue_instant state imsgs sender in
    { set_input =
        (fun cur ->
          let eq = equal_input in
          let old = S.value events.device.input in
          if eq old cur then Lwt.return cur else
            let e = S.changes events.device.input in
            let mode = t2mi_mode_to_raw storage#get.t2mi_mode in
            let req () = isend (Set_board_mode (cur, mode)) in
            wait ~eq cur req e)
    ; set_t2mi_mode =
        (fun mode ->
          let cur = t2mi_mode_to_raw mode in
          let old = t2mi_mode_to_raw @@ (S.value events.device.t2mi_mode) in
          let eq = equal_t2mi_mode_raw in
          if eq old cur then Lwt.return mode else
            let e = events.raw.t2mi_mode_raw in
            let req () = isend (Set_board_mode (storage#get.input, cur)) in
            wait ~eq cur req e
            >|= (fun _ -> push_events.t2mi_mode mode)
            >|= (fun () -> mode))
    ; set_jitter_mode =
        (fun cur ->
          let eq = Equal.option equal_jitter_mode in
          let old = (S.value config).jitter_mode in
          if eq old cur then Lwt.return cur else
            let e = E.map (fun (c : config) -> c.jitter_mode)
                    @@ S.changes config in
            let req () = isend (Set_jitter_mode cur) in
            wait ~eq cur req e
            >|= Fun.(tap push_events.jitter_mode))
    ; reset = (fun () -> isend Reset)
    ; get_section =
        (fun ?section ?table_id_ext ?id_ext_1 ?id_ext_2
             ~id ~table_id () ->
          let open SI_PSI_section.Dump in
          match Stream.find_by_id id @@ React.S.value events.streams with
          | None -> Lwt_result.fail Stream_not_found
          | Some s ->
             begin match s.orig_id with
             | TS_multi id ->
                let params =
                  { stream_id = id
                  ; table_id
                  ; section
                  ; table_id_ext
                  ; id_ext_1
                  ; id_ext_2
                  } in
                let req = Get_section { request_id = Board_serializer.get_id ()
                                      ; params } in
                let timer = Timer.steps ~step_duration 125. in
                Serializer.enqueue Parser.is_response state msgs sender req timer None
             (* XXX maybe other error here *)
             | _ -> Lwt_result.fail Stream_not_found
             end)
    ; get_t2mi_seq =
        (fun params ->
          let req = Get_t2mi_frame_seq { request_id = Board_serializer.get_id ()
                                       ; params } in
          let timer = Timer.steps ~step_duration
                      @@ float_of_int (params.seconds + 10) in
          Serializer.enqueue Parser.is_response state msgs sender req timer None)
    ; config =
        (fun () -> storage#get)
    ; get_devinfo =
        (fun () -> S.value events.device.info)
    ; get_streams =
        (fun ?ids ?incoming ?inputs () ->
          let v = React.S.value events.streams in
          let v = match incoming with
            | None | Some false -> v
            | Some true -> List.filter (is_incoming sources) v in
          let v = match inputs with
            | None | Some [] -> v
            | Some l ->
               List.filter (fun s ->
                   let eq = Topology.equal_topo_input in
                   match Stream.get_input s with
                   | None -> false
                   | Some i -> List.mem ~eq i l) v in
          let v = match ids with
            | None | Some [] -> v
            | Some l ->
               List.filter (fun (s : Stream.t) ->
                   let eq = Stream.ID.equal in
                   List.mem ~eq s.id l) v in
          Lwt.return v)
    ; get_ts_info =
        (fun () -> Lwt.return @@ React.S.value events.ts.info)
    ; get_pids =
        (fun () -> Lwt.return @@ React.S.value events.ts.pids)
    ; get_services =
        (fun () -> Lwt.return @@ React.S.value events.ts.services)
    ; get_tables =
        (fun () -> Lwt.return @@ React.S.value events.ts.tables)
    ; get_sections =
        (fun () -> Lwt.return @@ React.S.value events.ts.sections)
    ; get_t2mi_info =
        (fun () -> Lwt.return @@ React.S.value events.t2mi.structures)
    },
    msgs,
    imsgs

  let create sources sender (storage : config storage)
        step_duration streams_conv =
    let events, push_events =
      create_events storage streams_conv in
    let api, msgs, imsgs =
      create_api sources sender step_duration storage events push_events in
    (* let t2mi_locker = to_t2mi_locker events.device.t2mi_mode events.streams in *)
    (* E.keep
     * @@ E.map (fun (lock : bool) ->
     *        let state = events.device.state in
     *        let req x = Set_board_mode (storage#get.input, x) in
     *        if lock
     *        then (
     *          let def = t2mi_mode_raw_default in
     *          enqueue_instant state imsgs sender (req def))
     *        else (
     *          let raw = t2mi_mode_to_raw @@ (S.value events.device.t2mi_mode) in
     *          enqueue_instant state imsgs sender (req raw)))
     * @@ React.S.changes t2mi_locker; *)
    events,
    api,
    step sources msgs imsgs sender storage
      step_duration events push_events

end
