open Containers
open Board_qos_types
open Storage.Options
open Boards.Board
open Boards.Pools
open Common
open Board_parser

let status_timeout = 8 (* seconds *)
let detect_timeout = 3 (* seconds *)
let probes_timeout = 5 (* seconds *)

module Timer = Boards.Timer

(** Checks if the stream is received from board input *)
let is_incoming ({ t2mi; _ } : init) =
  fun (s : Stream.t) ->
  match s.orig_id with
  | TS_multi x ->
     let source_id = Multi_TS_ID.source_id x in
     source_id <> t2mi
  | _ -> false

let find_stream_by_multi_id (id : Stream.Multi_TS_ID.t)
      (streams : Stream.t list) =
  List.find_opt (fun (s : Stream.t) ->
      match s.orig_id with
      | TS_multi x -> Stream.Multi_TS_ID.equal x id
      | _ -> false) streams

let find_stream_by_id (id : Stream.ID.t) (streams : Stream.t list) =
  List.find_opt (fun (s : Stream.t) ->
      Stream.ID.equal s.id id) streams

let t2mi_mode_to_raw (mode : t2mi_mode option) =
  match mode with
  | None -> t2mi_mode_raw_default
  | Some { stream; pid; enabled; t2mi_stream_id } ->
     begin match stream.orig_id with
     | TS_multi x -> { pid; enabled; t2mi_stream_id; stream = x }
     | _ -> t2mi_mode_raw_default (* XXX maybe throw an exn here? *)
     end

module Acc = struct

  type probes =
    { board_errors : board_error list option
    ; bitrate : (Multi_TS_ID.t * Bitrate.t) list option
    ; ts_structs : (Multi_TS_ID.t * structure) list option
    ; t2mi_info : (Multi_TS_ID.t * T2mi_info.t list) list option
    ; jitter : jitter_raw option
    }

  let probes_empty =
    { board_errors = None
    ; bitrate = None
    ; ts_structs = None
    ; t2mi_info = None
    ; jitter = None
    }

  let merge_ts_probes ?(acc = []) l =
    List.fold_left (fun acc (id, x) ->
        List.Assoc.set ~eq:Multi_TS_ID.equal id x acc) acc l

  let merge_t2mi_info ?(acc = []) (id, x) =
    let open T2mi_info in
    let eq = Multi_TS_ID.equal in
    let f ((id, info) : t) = function
      | None -> Some [(id, info)]
      | Some (o : t list) ->
         let n = List.Assoc.set ~eq:equal_id id info o in
         Some n in
    List.Assoc.update ~eq ~f:(f x) id acc

  let cons (p : probes) = function
    | Board_errors x -> { p with board_errors = Some x }
    | Bitrate x ->
       let bitrate = Some (merge_ts_probes ?acc:p.bitrate x) in
       { p with bitrate }
    | Struct x ->
       let ts_structs = Some (merge_ts_probes ?acc:p.ts_structs x) in
       { p with ts_structs  }
    | T2mi_info x ->
       let t2mi_info = Some (merge_t2mi_info ?acc:p.t2mi_info x) in
       { p with t2mi_info }
    | Jitter x -> { p with jitter = Some x }

  type t =
    { group : group option
    ; events : board_event list
    ; probes : probes
    ; parts : (int * int, part list) List.Assoc.t
    ; bytes : Cstruct.t option
    }

  let empty =
    { group = None
    ; events = []
    ; probes = probes_empty
    ; parts = []
    ; bytes = None }

  let cons_probe (acc : t) x =
    { acc with probes = cons acc.probes x }

end

module Make(Logs : Logs.LOG) = struct

  type protocol_error =
    | Unexpected_info of devinfo
    | Unexpected_status

  module Parser = Board_parser.Make(Logs)

  let request_id = ref (-1)
  let jitter_ptr = ref (-1l)

  exception Protocol_invariant of protocol_error

  let get_id () =
    incr request_id;
    !request_id

  let wakeup_timeout (_, t) =
    t.pred `Timeout |> ignore

  let check_board_info rsps =
    match List.find_map (Parser.is_response Get_board_info) rsps with
    | None -> ()
    | Some x -> raise_notrace (Protocol_invariant (Unexpected_info x))

  module Events : sig
    open Stream
    val partition : Acc.t -> group list * Acc.t
    val get_req_stack : group -> group option -> probe_response probe_request list
    val to_ts_errors : group -> (Multi_TS_ID.t * Error.t list) list
    val to_t2mi_errors : group -> (Multi_TS_ID.t * Error.t list) list
    val handle : push_events -> events -> init -> Acc.t -> Acc.t
  end = struct

    let split_by l sep =
      let res, acc =
        List.fold_left (fun (res, acc) x ->
            if equal_board_event x sep
            then ((List.rev acc) :: res), [ ]
            else res, (x :: acc))
          ([ ], [ ]) l
      in (List.rev res), (List.rev acc)

    (** Returns merged groups and accumulator. Last group received is head of list *)
    let partition (acc : Acc.t) =
      let groups, rest = split_by acc.events `End_of_transmission in
      let groups =
        List.filter (function
            | `Status _ :: `Streams_event _ :: _ -> true
            | _ -> false) groups
        |> List.fold_left (fun (gps : group list) (x : board_event list) ->
               let prev_status = match gps with
                 | [ ] -> Option.(acc.group >|= (fun x -> x.status))
                 | x :: _ -> Some x.status in
               match x with
               | `Status status :: `Streams_event streams :: events ->
                  let status = { status with streams } in
                  ({ status; prev_status; events } : group) :: gps
               | _ -> assert false) []
      in groups, { acc with events = rest}

    let get_req_stack ({ status; _ } : group) (prev_t : group option) :
          probe_response probe_request list =
      let bitrate = if status.status.has_sync
                    then Some (Get_bitrates (get_id ())) else None in
      let jitter = match status.jitter_mode with
        | None -> None
        | Some m ->
           (* request for jitter only if required stream is present *)
           let eq = Stream.Multi_TS_ID.equal in
           if List.mem ~eq m.stream status.streams
           then Some (Get_jitter { request_id = get_id ()
                                 ; pointer    = !jitter_ptr })
           else None in
      let errors =
        if not status.errors then None else
          Some (Get_board_errors (get_id ())) in
      let ts_structs = match prev_t with
        | Some (old : group) ->
           if old.status.versions.ts_ver_com <> status.versions.ts_ver_com
           then Some (Get_ts_struct { stream = `All; request_id = get_id () })
           else None
        | None ->
           Some (Get_ts_struct { stream = `All; request_id = get_id () }) in
      let m = status.t2mi_mode in
      let t2mi_structs = match prev_t with
        | Some old ->
           (* XXX maybe we should request only those structures that really changed *)
           begin match (Equal.list Int.equal)
                         old.status.versions.t2mi_ver_lst
                         status.versions.t2mi_ver_lst with
           | true -> []
           | false ->
              List.map (fun id -> Get_t2mi_info { request_id = get_id ()
                                                ; stream = m.stream
                                                ; stream_id = id })
                status.t2mi_sync
           end
        | None ->
           if List.is_empty status.t2mi_sync then [] else
             List.map (fun id -> Get_t2mi_info { request_id = get_id ()
                                               ; stream = m.stream
                                               ; stream_id = id })
               status.t2mi_sync
      in
      t2mi_structs
      |> List.cons_maybe ts_structs
      |> List.cons_maybe bitrate
      |> List.cons_maybe jitter
      |> List.cons_maybe errors

    let to_ts_errors (g : group) =
      List.filter_map (function
          | `Ts_errors x -> Some x
          | _ -> None) g.events

    let to_t2mi_errors (g : group) =
      List.filter_map (function
          | `T2mi_errors x -> Some x
          | _ -> None) g.events

    (* let split_streams streams l =
     *   List.partition_map (fun (id, x) ->
     *       match find_stream_by_multi_id id streams with
     *       | None -> `Left (id, x)
     *       | Some s -> `Right (s.id, x)) l *)

    let merge_streams streams l =
      List.filter_map (fun (id, x) ->
          match find_stream_by_multi_id id streams with
          | None ->
             Logs.err (fun m -> m "Not found a stream for data!");
             None
          | Some s -> Some (s.id, x)) l

    let to_raw_stream ({ input; t2mi } : init)
          (mode : t2mi_mode_raw)
          (i : input) (id : Multi_TS_ID.t) =
      let open Stream.Source in
      let open Stream.Raw in
      let source_id = Multi_TS_ID.source_id id in
      let stream_id = Multi_TS_ID.stream_id id in
      let spi_id = input_to_int SPI in
      let asi_id = input_to_int ASI in
      let src = match source_id, stream_id with
        | src, id when src = input && id = spi_id -> `Spi
        | src, id when src = input && id = asi_id -> `Asi
        | src, id when src = t2mi -> `T2mi id
        | _ -> begin match i with SPI -> `Spi | ASI -> `Asi end in
      let source = match src, mode with
        | `T2mi plp, { stream
                     ; t2mi_stream_id = stream_id
                     ; enabled = true
                     ; _ } ->
           let node = Stream (TS_multi stream) in
           let info = T2MI { stream_id; plp } in
           Some { node; info }
        | `T2mi _, _ -> None
        | `Spi, _ -> Some { node = Port spi_id; info = SPI }
        | `Asi, _ -> Some { node = Port asi_id; info = ASI }
        | `Unknown, _ -> None
      in
      match source with
      | None -> None
      | Some source ->
         let (typ : Stream.stream_type) =
           if Multi_TS_ID.equal id mode.stream && mode.enabled
           then T2MI else TS in
         Some { id = TS_multi id; source; typ }

    let handle (pe : push_events)
          (events : events)
          (sources : init)
          (acc : Acc.t) =
      let (({ status = { status; t2mi_mode; streams = ids; input; _ }
            ; _ } : group) as group) =
        Option.get_exn acc.group in
      let timestamp = status.timestamp in
      let streams = React.S.value events.streams in
      (* Push input *)
      pe.input input;
      (* Push status *)
      pe.status status;
      (* Push t2mi mode raw *)
      pe.t2mi_mode_raw group.status.t2mi_mode;
      (* Push raw streams *)
      pe.raw_streams
      @@ List.filter_map (to_raw_stream sources t2mi_mode input) ids;
      (* Push board errors *)
      begin match acc.probes.board_errors with
      | None -> ()
      | Some x ->
         Logs.warn (fun m ->
             m "got board errors: [%s]"
             @@ String.concat "; "
             @@ List.map (fun { err_code; count; _ } ->
                    Printf.sprintf "%d: %d" err_code count) x);
         pe.board_errors x
      end;
      (* Push jitter *)
      begin match acc.probes.jitter with
      | None -> ()
      | Some x ->
         jitter_ptr := x.next_ptr;
         pe.jitter x.measures
      end;
      (* Stream dependent events *)
      (* Push TS bitrates *)
      begin match acc.probes.bitrate with
      | None -> ()
      | Some x ->
         let v = merge_streams streams x in
         List.map (Pair.map2 (make_timestamped timestamp)) v
         |> pe.bitrates
      end;
      (* Push TS structures *)
      (* FIXME do smth if corresponding stream is not found *)
      begin match acc.probes.ts_structs with
      | None -> ()
      | Some x ->
         let v = merge_streams streams x in
         let map2 = Pair.map2 in
         let info = List.map (map2 (fun (x : structure) ->
                                  make_timestamped timestamp x.info)) v in
         let pids = List.map (map2 (fun (x : structure) ->
                                  make_timestamped timestamp x.pids)) v in
         let tbls = List.map (map2 (fun (x : structure) ->
                                  make_timestamped timestamp x.tables)) v in
         let srvs = List.map (map2 (fun (x : structure) ->
                                  make_timestamped timestamp x.services)) v in
         pe.info info;
         pe.pids pids;
         pe.tables tbls;
         pe.services srvs;
      end;
      (* Push TS errors *)
      begin match to_ts_errors group with
      | [] -> ()
      | v ->
         merge_streams streams v
         |> List.map (Pair.map2 (List.map (make_timestamped timestamp)))
         |> pe.ts_errors
      end;
      (* Push T2-MI info *)
      (* FIXME do smth if corresponding stream is not found *)
      begin match acc.probes.t2mi_info with
      | None -> ()
      | Some x ->
         merge_streams streams x
         |> List.map (Pair.map2 (make_timestamped timestamp))
         |> pe.t2mi_info
      end;
      (* Push T2-MI errors *)
      begin match to_t2mi_errors group with
      | [] -> ()
      | v ->
         merge_streams streams v
         |> List.map (Pair.map2 (List.map (make_timestamped timestamp)))
         |> pe.t2mi_errors
      end;
      { acc with probes = Acc.probes_empty }

  end

  let probe_req_to_string = function
    | Get_board_errors _ -> "Board errors"
    | Get_jitter _ -> "Jitter"
    | Get_ts_struct _ -> "TS structure"
    | Get_bitrates _ -> "TS bitrates"
    | Get_t2mi_info _ -> "T2-MI info"

  let probe_rsp_to_string = function
    | Board_errors _ -> "Board errors"
    | Bitrate _ -> "TS bitrates"
    | Struct _ -> "TS structures"
    | T2mi_info _ -> "T2-MI info"
    | Jitter _ -> "Jitter"

  let send_msg (type a) sender (msg : a request) : unit Lwt.t =
    (match msg with
     | Get_board_info ->
        Logs.debug (fun m -> m "requesting board info");
        Parser.Get_board_info.serialize ()
     | Get_board_mode ->
        Logs.debug (fun m -> m "requesting board mode");
        Parser.Get_board_mode.serialize ()
     | Get_t2mi_frame_seq x ->
        Logs.debug (fun m -> m "requesting t2mi frame sequence: %s"
                             @@ show_t2mi_frame_seq_req x);
        Parser.Get_t2mi_frame_seq.serialize x
     | Get_section x ->
        Logs.debug (fun m -> m "requesting section: %s"
                             @@ show_section_req x);
        Parser.Get_section.serialize x)
    |> sender

  let send_event (type a) sender (msg : a probe_request) : unit Lwt.t =
    (match msg with
     | Get_board_errors id ->
        Logs.debug (fun m -> m "requesting board errors");
        Parser.Get_board_errors.serialize id
     | Get_jitter req ->
        Logs.debug (fun m -> m "requesting jitter: %s"
                             @@ show_jitter_req req);
        Parser.Get_jitter.serialize req
     | Get_ts_struct req ->
        Logs.debug (fun m -> m "requesting ts structs: %s"
                             @@ show_ts_struct_req req);
        Parser.Get_ts_structs.serialize req
     | Get_bitrates req ->
        Logs.debug (fun m -> m "requesting bitrates");
        Parser.Get_bitrates.serialize req
     | Get_t2mi_info req ->
        Logs.debug (fun m -> m "requesting t2mi info: %s"
                             @@ show_t2mi_info_req req);
        Parser.Get_t2mi_info.serialize req)
    |> sender

  let send_instant (type a) sender (msg : a instant_request) : unit Lwt.t =
    (match msg with
     | Reset ->
        Logs.debug (fun m -> m "requesting reset");
        to_complex_req ~msg_code:0x0111 ~body:Cstruct.empty ()
     | Set_board_init x ->
        Logs.debug (fun m -> m "requesting board init");
        to_set_board_init_req x
     | Set_board_mode (i, m) ->
        Logs.debug (fun m -> m "requesting board mode setup");
        to_set_board_mode_req (i, m)
     | Set_jitter_mode x ->
        Logs.debug (fun m -> m "requesting set jitter mode setup");
        to_set_jitter_mode_req x)
    |> sender

  let enqueue (type a) state msgs sender (msg : a request) timeout exn : a Lwt.t =
    (* no instant msgs *)
    match React.S.value state with
    | `Fine ->
       let t, w = Lwt.wait () in
       let pred = function
         | `Timeout -> Lwt.wakeup_exn w (Failure "msg timeout"); None
         | l  -> Option.(Parser.is_response msg l >|= Lwt.wakeup w) in
       let send = fun () -> send_msg sender msg in
       msgs := Await_queue.append !msgs { send; pred; timeout; exn };
       t
    | _ -> Lwt.fail (Failure "board is not responding")

  let enqueue_instant (type a) state msgs sender (msg : a instant_request) : unit Lwt.t =
    let open Lwt.Infix in
    match React.S.value state with
    | `Fine ->
       let t, w = Lwt.wait () in
       let send = fun () -> (send_instant sender msg)
                            >>= (fun x -> Lwt.return @@ Lwt.wakeup w x) in
       let pred = fun _ -> Some () in
       msgs := Queue.append !msgs { send; pred; timeout = 0; exn = None };
       t
    | _ -> Lwt.fail (Failure "board is not responding")

  let push_state (pe : push_events) state =
    pe.bitrates [];
    pe.state state

  let step sources msgs imsgs sender (storage : config storage) step_duration
        (events : events) (pe : push_events) =

    let board_info_err_msg =
      "board info was received during normal \
       operation, restarting..." in
    let no_status_msg t =
      Printf.sprintf "no status received for %d seconds, restarting..."
        (Timer.period t) in

    let deserialize ~with_init_exn (acc : Acc.t) recvd =
      let events, probes, rsps, parts, bytes =
        Parser.deserialize acc.parts (concat_acc acc.bytes recvd) in
      (* raises exception if board info message is found *)
      if with_init_exn then check_board_info rsps;
      let events = acc.events @ events in
      probes, rsps, { acc with events; parts; bytes } in

    let handle_msgs rsps =
      if Await_queue.has_pending !msgs
      then (msgs := fst @@ Await_queue.responsed !msgs rsps;
            let new_msgs,tout = Await_queue.step !msgs in
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
      send_msg sender Get_board_info |> Lwt.ignore_result;
      `Continue (step_detect (Timer.create ~step_duration detect_timeout) Acc.empty)

    and step_detect (timer : Timer.t) (acc : Acc.t) recvd =
      try
        let _, rsps, acc = deserialize ~with_init_exn:false acc recvd in
        match List.find_map (Parser.is_response Get_board_info) rsps with
        | None -> `Continue (step_detect (Timer.step timer) acc)
        | Some info -> `Continue (step_init info acc)
      with Timer.Timeout t ->
        (Logs.warn (fun m ->
             m "connection is not established after %d seconds, \
                restarting" (Timer.period t));
         first_step ())

    and step_init devinfo (acc : Acc.t) _ =
      push_state pe `Init;
      pe.devinfo (Some devinfo);
      let { t2mi_mode; input; jitter_mode } : config = storage#get in
      send_instant sender (Set_board_init sources)
      |> Lwt.ignore_result;
      let t2mi_mode_raw = t2mi_mode_to_raw t2mi_mode in
      send_instant sender (Set_board_mode (input, t2mi_mode_raw))
      |> Lwt.ignore_result;
      send_instant sender (Set_jitter_mode jitter_mode)
      |> Lwt.ignore_result;
      Logs.info (fun m -> m "connection established, waiting \
                             for 'status' message");
      let timer = Timer.create ~step_duration status_timeout in
      `Continue (step_ok_idle timer acc)

    and step_ok_idle (timer : Timer.t) (acc : Acc.t) recvd =
      try
        let _, rsps, acc = deserialize ~with_init_exn:true acc recvd in
        let () = handle_msgs rsps in
        let () = if not (Queue.empty !imsgs)
                 then Queue.send !imsgs () |> Lwt.ignore_result in
        let () = imsgs := Queue.next !imsgs in
        (match Events.partition acc with
         | [], acc -> `Continue (step_ok_idle (Timer.step timer) acc)
         | [group], acc ->
            if Option.is_none acc.group
            then (Logs.info (fun m -> m "initialization done!");
                  push_state pe `Fine);
            Logs.debug (fun m ->
                m "received group: load=%g%%" group.status.status.load);
            let stack = Events.get_req_stack group acc.group in
            let pool =
              List.map (fun req ->
                  { send = (fun () -> send_event sender req)
                  ; pred = Parser.is_probe_response req
                  ; timeout = Timer.steps ~step_duration probes_timeout
                  ; exn = None }) stack
              |> Pool.create in
            Logs.debug (fun m ->
                let pre = "prepared stack of probe requests" in
                let stk = String.concat "; "
                          @@ List.map probe_req_to_string stack in
                m "%s: [%s]" pre stk);
            step_ok_probes_send pool (Timer.reset timer)
              { acc with group = Some group }
         | _ -> raise_notrace (Protocol_invariant Unexpected_status))
      with
      | Protocol_invariant (Unexpected_info info) ->
         Logs.warn (fun m -> m "%s" board_info_err_msg);
         push_state pe `No_response;
         `Continue (step_init info Acc.empty)
      | Protocol_invariant Unexpected_status ->
         Logs.warn (fun m -> m "Got more than one status at once, \
                                restarting...");
         first_step ()
      | Timer.Timeout t ->
         Logs.warn (fun m -> m "%s" (no_status_msg t));
         first_step ()

    and step_ok_probes_send pool (timer : Timer.t) (acc : Acc.t) =
      if Pool.empty pool
      then `Continue (step_ok_idle (Timer.reset timer) acc)
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
          | _ -> raise_notrace (Protocol_invariant Unexpected_status) in
        begin match Pool.responsed pool probes with
        | None -> `Continue (step_ok_probes_wait (Pool.step pool) timer acc)
        | Some x ->
           Logs.debug (fun m ->
               m "got probe response: '%s'" @@ probe_rsp_to_string x);
           let acc = Acc.cons_probe acc x in
           begin match Pool.last pool with
           | true ->
              let (acc : Acc.t) =
                Events.handle pe events sources acc in
              `Continue (step_ok_idle timer acc)
           | false -> step_ok_probes_send (Pool.next pool) timer acc
           end
        end
      with
      | Protocol_invariant Unexpected_status ->
         Logs.warn (fun m -> m "got status while waiting for probes. \
                                Looks like an overload, restarting...");
         first_step ()
      | Protocol_invariant (Unexpected_info info) ->
         Logs.warn (fun m -> m "%s" board_info_err_msg);
         push_state pe `No_response;
         `Continue (step_init info Acc.empty);
      | Timer.Timeout t ->
         Logs.warn (fun m -> m "%s" (no_status_msg t));
         first_step ()
      | Timeout ->
         Logs.warn (fun m ->
             m "timeout while waiting for probe \
                response, restarting...");
         first_step ()

    in
    first_step ()

  open Lwt_react

  (* T2-MI mode signal, contains value to apply to the board *)
  (* FIXME check this *)
  let to_t2mi_locker
        (mode : t2mi_mode option signal)
        (streams : Stream.t list signal) =
    let find (streams : Stream.t list) = function
      | None -> None
      | Some (stream : Stream.t) ->
         List.find_opt (fun (s : Stream.t) ->
             not @@ Stream.ID.equal s.id stream.id
             && Stream.equal_container_id s.orig_id stream.orig_id) streams in
    React.S.l2 (fun streams (mode : t2mi_mode option) ->
        let stream = Option.map (fun (x : t2mi_mode) -> x.stream) mode in
        match find streams stream with
        | None -> false
        | Some _ -> true) streams mode

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
    let state, set_state  = S.create `No_response in
    let devinfo, set_devinfo = S.create None in
    let status, set_status = E.create () in
    let input, set_input = S.create storage#get.input in
    let hw_errors, set_hw_errors = E.create () in
    let raw_streams, set_raw_streams = S.create [] in
    let ts_info, set_ts_info = S.create [] in
    let services, set_services = S.create [] in
    let tables, set_tables = S.create [] in
    let pids, set_pids = S.create [] in
    let bitrates, set_bitrates = E.create () in
    let t2mi_info, set_t2mi_info = S.create [] in
    let ts_errors, set_ts_errors = E.create () in
    let t2mi_errors, set_t2mi_errors = E.create () in
    let t2mi_mode_raw, set_t2mi_mode_raw = E.create () in
    let t2mi_mode, set_t2mi_mode = S.create storage#get.t2mi_mode in
    let jitter_mode, set_jitter_mode = S.create storage#get.jitter_mode in
    let config =
      S.l3 (fun input t2mi_mode jitter_mode ->
          { input; t2mi_mode; jitter_mode }) input t2mi_mode jitter_mode in
    let e_pcr, pcr_push = E.create () in
    let e_pcr_s, pcr_s_push = E.create () in

    let streams = streams_conv raw_streams in
    let sections =
      S.map (List.map (fun (id, { timestamp; data }) ->
                 id, { timestamp; data = to_sections data })) tables in
    Lwt_react.S.keep @@ S.map (fun c -> storage#store c) config;
    let device =
      { state
      ; info = devinfo
      ; t2mi_mode
      ; jitter_mode
      ; config
      ; t2mi_mode_raw
      ; input
      ; status
      ; errors = hw_errors
      } in
    let ts =
      { info = ts_info
      ; services
      ; sections
      ; tables
      ; pids
      ; bitrates
      ; errors = ts_errors
      } in
    let t2mi =
      { structures = t2mi_info
      ; errors = t2mi_errors
      } in
    let jitter =
      { session = E.changes ~eq:Jitter.equal_session e_pcr_s
      ; jitter = e_pcr
      } in
    let (events : events) =
      { device
      ; ts
      ; t2mi
      ; streams
      ; jitter
      } in
    let (push_events : push_events) =
      { devinfo = set_devinfo
      ; input = set_input
      ; status = set_status
      ; t2mi_mode_raw = set_t2mi_mode_raw
      ; t2mi_mode = set_t2mi_mode
      ; jitter_mode = set_jitter_mode
      ; state = set_state
      ; raw_streams = set_raw_streams
      ; ts_errors = set_ts_errors
      ; t2mi_errors = set_t2mi_errors
      ; board_errors = set_hw_errors
      ; info = set_ts_info
      ; services = set_services
      ; tables = set_tables
      ; pids = set_pids
      ; bitrates = set_bitrates
      ; t2mi_info = set_t2mi_info
      ; jitter = pcr_push
      ; jitter_session = pcr_s_push
      }
    in
    events, push_events

  let wait = fun ~eq t s v ->
    let open Lwt.Infix in
    if eq (S.value s) v then Lwt.return v else
      t ()
      >>= (fun () ->
      if eq (S.value s) v then Lwt.return v
      else Lwt.pick [ E.next @@ S.changes s
                    ; Lwt_unix.timeout (float_of_int status_timeout)])
      >>= (fun x  ->
      React.S.stop s;
      if eq x v then Lwt.return x
      else Lwt.fail @@ Failure "got unexpected value")

  let create_api sources sender step_duration
        (storage : config storage) events (push_events : push_events) =
    let open Lwt.Infix in
    let { device = { config; state; _ }; _ }  = events in
    let msgs = ref (Await_queue.create []) in
    let imsgs = ref (Queue.create []) in
    let isend = enqueue_instant state imsgs sender in
    { set_input =
        (fun i ->
          let eq = equal_input in
          let s = S.map (fun (c : config) -> c.input) config in
          let mode = t2mi_mode_to_raw storage#get.t2mi_mode in
          let t () = isend (Set_board_mode (i, mode)) in
          wait ~eq t s i)
    ; set_t2mi_mode =
        (fun mode ->
          let eq = equal_t2mi_mode_raw in
          (* current value *)
          let v = t2mi_mode_to_raw @@ (S.value events.device.t2mi_mode) in
          let s = S.hold v @@ events.device.t2mi_mode_raw in
          let raw = t2mi_mode_to_raw mode in
          let t () = isend (Set_board_mode (storage#get.input, raw)) in
          wait ~eq t s raw
          >|= (fun _ -> push_events.t2mi_mode mode)
          >|= (fun () -> mode))
    ; set_jitter_mode =
        (fun mode ->
          let eq = Equal.option equal_jitter_mode in
          let s = S.map (fun (c : config) -> c.jitter_mode) config in
          let t () = isend (Set_jitter_mode mode) in
          wait ~eq t s mode
          >|= (fun _ -> push_events.jitter_mode mode)
          >|= (fun () -> mode))
    ; reset = (fun () -> isend Reset)
    ; get_section =
        (fun ?section ?table_id_ext ?id_ext_1 ?id_ext_2
             ~id ~table_id () ->
          let open SI_PSI_section.Dump in
          match find_stream_by_id id @@ React.S.value events.streams with
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
                let req = Get_section { request_id = get_id (); params } in
                let timer = Timer.steps ~step_duration 125 in
                enqueue state msgs sender req timer None
             (* XXX maybe other error here *)
             | _ -> Lwt_result.fail Stream_not_found
             end)
    ; get_t2mi_seq =
        (fun params ->
          let req = Get_t2mi_frame_seq { request_id = get_id (); params } in
          let timer = Timer.steps ~step_duration (params.seconds + 10) in
          enqueue state msgs sender req timer None)
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
    let t2mi_locker = to_t2mi_locker events.device.t2mi_mode events.streams in
    E.keep
    @@ E.map (fun (lock : bool) ->
           let state = events.device.state in
           let req x = Set_board_mode (storage#get.input, x) in
           if lock
           then (
             let def = t2mi_mode_raw_default in
             enqueue_instant state imsgs sender (req def))
           else (
             let raw = t2mi_mode_to_raw @@ (S.value events.device.t2mi_mode) in
             enqueue_instant state imsgs sender (req raw)))
    @@ React.S.changes t2mi_locker;
    events,
    api,
    step sources msgs imsgs sender storage
      step_duration events push_events

end
