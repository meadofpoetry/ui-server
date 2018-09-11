open Containers
open Board_types
open Types
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
let is_incoming ({ t2mi; _ } : Types.init) =
  fun (s : Stream.t) ->
  match s.orig_id with
  | TS_multi x ->
     let parsed = Multi_TS_ID.parse x in
     if parsed.source_id = t2mi
     then false else true
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

  let merge_ts_probes ~acc l =
    List.fold_left (fun acc (id, x) ->
        List.Assoc.set ~eq:Multi_TS_ID.equal id x acc) acc l

  let merge_t2mi_info ~acc (id, x) =
    let open Streams.T2MI in
    let eq = Multi_TS_ID.equal in
    let f (x : structure) = function
      | None -> Some x
      | Some (o : structure) ->
         Some { o with streams = o.streams @ x.streams } in
    List.Assoc.update ~eq ~f:(f x) id acc

  type probes =
    { board_errors : (board_error list) list
    ; bitrate : (Multi_TS_ID.t * Streams.TS.bitrate) list
    ; ts_structs : (Multi_TS_ID.t * Streams.TS.structure) list
    ; t2mi_info : (Multi_TS_ID.t * Streams.T2MI.structure) list
    ; jitter : Types.jitter_raw list
    }
  let probes_empty =
    { board_errors = []
    ; bitrate = []
    ; ts_structs = []
    ; t2mi_info = []
    ; jitter = []
    }
  let cons (p : probes) = function
    | Board_errors x ->
       { p with board_errors = x :: p.board_errors }
    | Bitrate x ->
       { p with bitrate = merge_ts_probes ~acc:p.bitrate x }
    | Struct x ->
       { p with ts_structs = merge_ts_probes ~acc:p.ts_structs x }
    | T2mi_info x ->
       { p with t2mi_info = merge_t2mi_info ~acc:p.t2mi_info x }
    | Jitter x ->
       { p with jitter = x :: p.jitter }

  type t =
    { group : Types.group option
    ; events : Types.event list
    ; probes : probes
    ; await_probes : probes
    ; parts : (int * int, part list) List.Assoc.t
    ; bytes : Cstruct.t option
    }
  let empty =
    { group = None
    ; events = []
    ; probes = probes_empty
    ; await_probes = probes_empty
    ; parts = []
    ; bytes = None }

  let cons_probe (acc : t) x =
    { acc with probes = cons acc.probes x }
end

module Make(Logs : Logs.LOG) = struct

  let request_id = ref (-1)
  let jitter_ptr = ref (-1l)

  exception Status_timeout
  exception Unexpected_init of devinfo

  let get_id () =
    incr request_id;
    !request_id

  let wakeup_timeout (_, t) = t.pred `Timeout |> ignore

  let check_board_info rsps =
    match List.find_map (is_response Get_board_info) rsps with
    | None -> ()
    | Some x -> raise_notrace (Unexpected_init x)

  module Events : sig
    open Stream
    val partition : Acc.t -> group list * Acc.t
    val update_versions : Acc.t -> group -> Acc.t
    val get_req_stack : group -> group option -> probe_response probe_request list
    val to_ts_errors : group -> (Multi_TS_ID.t * Errors.t list) list
    val to_t2mi_errors : group -> (Multi_TS_ID.t * Errors.t list) list
  end = struct

    let update_versions (acc : Acc.t) (t : group) : Acc.t = match acc.group with
      | None -> acc
      | Some from ->
         let status = { t.status with versions = from.status.versions } in
         let group = { t with status } in
         { acc with group = Some group }

    let split_by l sep =
      let res, acc =
        List.fold_left (fun (res, acc) x ->
            if equal_event x sep
            then ((List.rev acc) :: res), [ ]
            else res, (x :: acc))
          ([ ], [ ]) l
      in (List.rev res), (List.rev acc)

    (* let t2mi_ver_diff (old : group) =
     *   List.foldi (fun acc i x ->
     *       if (x <> (List.nth old.status.versions.t2mi_ver_lst i))
     *          && List.mem ~eq:(=) i status.t2mi_sync
     *       then i :: acc else acc)
     *     [] status.versions.t2mi_ver_lst in *)

    (** Returns merged groups and accumulator. Last group received is head of list *)
    let partition (acc : Acc.t) =
      let groups, rest = split_by acc.events `End_of_transmission in
      let groups =
        List.filter (function
            | `Status _ :: `Streams_event _ :: _ -> true
            | _ -> false) groups
        |> List.fold_left (fun (gps : group list) (x : event list) ->
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
      (* let ts_structs = match prev_t with
       *   | Some old ->
       *      let old_version = old.status.versions.ts_ver_lst in
       *      let old_streams = old.status.streams in
       *      let new_version = status.versions.ts_ver_lst in
       *      let new_streams = status.streams in
       *      let streams =
       *        List.foldi (fun acc i id ->
       *            match List.find_idx (fun x -> Stream.equal_id id x)
       *                    old_streams with
       *            | Some (idx, o) ->
       *               (try
       *                  let ov = List.get_at_idx_exn idx old_version in
       *                  let nv = List.get_at_idx_exn i new_version in
       *                  if ov <> nv then id :: acc else acc
       *                with _ -> acc)
       *            | None -> id :: acc) [] new_streams in
       *      List.map (fun x ->
       *          Get_ts_struct { request_id = get_id ()
       *                        ; stream     = `Single x }) streams
       *   | None -> [ Get_ts_struct { request_id = get_id ()
       *                             ; stream     = `All } ] in *)
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
      List.filter_map (function `Ts_errors x -> Some x
                              | _ -> None) g.events

    let to_t2mi_errors (g : group) =
      List.filter_map (function `T2mi_errors x -> Some x
                              | _ -> None) g.events

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
        Get_board_info.serialize ()
     | Get_board_mode ->
        Logs.debug (fun m -> m "requesting board mode");
        Get_board_mode.serialize ()
     | Get_t2mi_frame_seq x ->
        Logs.debug (fun m -> m "requesting t2mi frame sequence: %s"
                             @@ show_t2mi_frame_seq_req x);
        Get_t2mi_frame_seq.serialize x
     | Get_section x ->
        Logs.debug (fun m -> m "requesting section: %s"
                             @@ show_section_req x);
        Get_section.serialize x)
    |> sender

  let send_event (type a) sender (msg : a probe_request) : unit Lwt.t =
    (match msg with
     | Get_board_errors id ->
        Logs.debug (fun m -> m "requesting board errors");
        Get_board_errors.serialize id
     | Get_jitter req ->
        Logs.debug (fun m -> m "requesting jitter: %s"
                             @@ show_jitter_req req);
        Get_jitter.serialize req
     | Get_ts_struct req ->
        Logs.debug (fun m -> m "requesting ts structs: %s"
                             @@ show_ts_struct_req req);
        Get_ts_structs.serialize req
     | Get_bitrates req ->
        Logs.debug (fun m -> m "requesting bitrates");
        Get_bitrates.serialize req
     | Get_t2mi_info req ->
        Logs.debug (fun m -> m "requesting t2mi info: %s"
                             @@ show_t2mi_info_req req);
        Get_t2mi_info.serialize req)
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
         | l  -> Option.( is_response msg l >|= Lwt.wakeup w ) in
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

  let push_structs (pe : push_events)
        (x : (Stream.t * Streams.TS.structure) list) =
    let open Streams.TS in
    let map_snd f l = List.map (fun (s, x) -> s, f x) l in
    pe.info @@ map_snd (fun x -> x.info) x;
    pe.services @@ map_snd (fun x -> x.services) x;
    pe.tables @@ map_snd (fun x -> x.tables) x;
    pe.pids @@ map_snd (fun x -> x.pids) x

  type t2mi = (Stream.t * Streams.T2MI.structure) list [@@deriving show]

  let handle_probes (pe : push_events)
        (streams : Stream.t list)
        (acc : Acc.t) =
    let split l =
        List.partition_map (fun (id, x) ->
            match find_stream_by_multi_id id streams with
            | None -> `Left (id, x)
            | Some s -> `Right (s, x)) l in
    (* Try to merge previous *)
    let { t2mi_info; bitrate; ts_structs; _ } : Acc.probes =
      acc.await_probes in
    let _, rdy = split t2mi_info in
    pe.t2mi_info rdy;
    let _, rdy = split bitrate in
    pe.bitrates rdy;
    let _, rdy = split ts_structs in
    (push_structs pe) rdy;
    (* Streams independent probes *)
    List.iter (fun x ->
        Logs.warn (fun m ->
            let s = String.concat ",\n"
                    @@ List.map show_board_error x in
            m "got board errors: %s" s);
        pe.board_errors x) acc.probes.board_errors;
    List.iter (fun x ->
        (* XXX pointer update requires right order *)
        jitter_ptr := x.next_ptr;
        pe.jitter x.measures) acc.probes.jitter;
    (* Streams dependent probes *)
    let t2mi_info =
      let await, rdy = split acc.probes.t2mi_info in
      pe.t2mi_info rdy;
      await in
    let bitrate =
      let await, rdy = split acc.probes.bitrate in
      pe.bitrates rdy;
      await in
    let ts_structs =
      let await, rdy = split acc.probes.ts_structs in
      (push_structs pe) rdy;
      await in
    let await =
      { Acc.probes_empty with t2mi_info; ts_structs; bitrate } in
    { acc with probes = Acc.probes_empty
             ; await_probes = await }

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

    let module Parser = Board_parser.Make(Logs) in

    let deserialize ~with_init_exn (acc : Acc.t) recvd =
      let events, probes, rsps, parts, bytes =
        Parser.deserialize acc.parts (concat_acc acc.bytes recvd) in
      (* raises exception if board info message is found *)
      if with_init_exn then check_board_info rsps;
      let events = acc.events @ events in
      probes, rsps, { acc with events; parts; bytes } in

    let log_groups groups =
      Logs.debug (fun m ->
          let pre = "received group(s)" in
          let gps =
            List.map show_group groups
            |> String.concat "\n\n"
          in
          m "%s: \n%s" pre gps) in

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
        match List.find_map (is_response Get_board_info) rsps with
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
         | (group :: _) as groups, acc ->
            if Option.is_none acc.group
            then (Logs.info (fun m -> m "initialization done!");
                  push_state pe `Fine);
            log_groups groups;
            List.iter (fun x -> pe.group x) @@ List.rev groups;
            let stack = Events.get_req_stack group acc.group in
            let pool =
              List.map (fun req ->
                  { send = (fun () -> send_event sender req)
                  ; pred = is_probe_response req
                  ; timeout = Timer.steps ~step_duration probes_timeout
                  ; exn = None }) stack
              |> Pool.create in
            Logs.debug (fun m ->
                let pre = "prepared stack of probe requests" in
                let stk = String.concat "; "
                          @@ List.map probe_req_to_string stack in
                m "%s: [%s]" pre stk);
            step_ok_probes_send pool (Timer.reset timer)
              { acc with group = Some group })
      with
      | Unexpected_init info ->
         Logs.warn (fun m -> m "%s" board_info_err_msg);
         push_state pe `No_response;
         `Continue (step_init info Acc.empty)
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
        let () = handle_msgs rsps in
        let () = if not (Queue.empty !imsgs)
                 then Queue.send !imsgs () |> Lwt.ignore_result in
        let () = imsgs := Queue.next !imsgs in
        let timer, acc = match Events.partition acc with
          | [], acc -> (Timer.step timer), acc
          | (hd :: _) as l, acc ->
             log_groups l;
             List.iter (fun x -> pe.group x) @@ List.rev l;
             (Timer.reset timer), (Events.update_versions acc hd) in
        begin match Pool.responsed pool probes with
        | None -> `Continue (step_ok_probes_wait (Pool.step pool) timer acc)
        | Some x ->
           Logs.debug (fun m ->
               let pre = "got probe response" in
               let rsp = probe_rsp_to_string x in
               m "%s: '%s'" pre rsp);
           let acc = Acc.cons_probe acc x in
           begin match Pool.last pool with
           | true ->
              let acc : Acc.t =
                handle_probes pe (React.S.value events.streams) acc in
              `Continue (step_ok_idle timer acc)
           | false -> step_ok_probes_send (Pool.next pool) timer acc
           end
        end
      with
      | Unexpected_init info ->
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

  let to_raw_stream ({ input; t2mi } : Types.init)
        (mode : t2mi_mode_raw)
        (i : input) (id : Multi_TS_ID.t) =
    let open Stream.Source in
    let open Stream.Raw in
    let parsed = Multi_TS_ID.parse id in
    let spi_id = input_to_int SPI in
    let asi_id = input_to_int ASI in
    let src = match parsed.source_id, parsed.stream_id with
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
       let typ : Stream.stream_type =
         if Multi_TS_ID.equal id mode.stream && mode.enabled
         then T2MI else TS in
       Some { id = TS_multi id; source; typ }

  let to_raw_streams_s sources (group : group event)
        (state : Topology.state signal) : Stream.Raw.t list signal =
    let e_group =
      E.map (fun ({ status = { streams; t2mi_mode; input; _ }; _ } : group) ->
          List.filter_map (to_raw_stream sources t2mi_mode input) streams)
        group in
    let eq = Equal.list Stream.Raw.equal in
    let e_state = S.changes @@ S.map (fun _ -> []) state in
    React.E.select [ e_state; e_group ]
    |> S.hold ~eq []

  (* T2-MI mode signal, contains value to apply to the board *)
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

  let create_events sources (storage : config storage) streams_conv =
    let state, set_state  = S.create `No_response in
    let devinfo, set_devinfo = S.create None in
    let group, set_group = E.create () in
    let hw_errors, set_hw_errors = E.create () in
    let ts_info, set_ts_info = S.create [] in
    let services, set_services = S.create [] in
    let tables, set_tables = S.create [] in
    let pids, set_pids = S.create [] in
    let bitrates, set_bitrates = E.create () in
    let t2mi_info, set_t2mi_info = E.create () in
    let t2mi_mode, set_t2mi_mode = S.create storage#get.t2mi_mode in
    let jitter_mode, set_jitter_mode = S.create storage#get.jitter_mode in
    let input =
      S.hold storage#get.input
      @@ E.changes ~eq:equal_input
      @@ E.map (fun (x : group) -> x.status.input) group in
    let config =
      S.l3 (fun input t2mi_mode jitter_mode ->
          { input; t2mi_mode; jitter_mode }) input t2mi_mode jitter_mode in
    let e_pcr, pcr_push = E.create () in
    let e_pcr_s, pcr_s_push = E.create () in

    let streams = streams_conv (to_raw_streams_s sources group state) in
    let fmap l streams =
      List.filter_map (fun (id, x) ->
          match find_stream_by_multi_id id streams with
          | None -> None
          | Some s -> Some (s, x)) l in
    let map ~eq s =
      (* S.l2 fmap s streams
       * |>  *)
      S.diff (fun _new old ->
          List.filter (fun (stream, s) ->
              match List.Assoc.get ~eq:Stream.equal stream old with
              | Some x -> not @@ eq x s
              | None -> true) _new) s in
    let map_errors f e =
      S.sample (fun l streams -> fmap (f l) streams) e streams in
    Lwt_react.S.keep @@ S.map (fun c -> storage#store c) config;
    let open Streams.TS in
    let device =
      { state
      ; info = devinfo
      ; t2mi_mode
      ; jitter_mode
      ; config
      ; t2mi_mode_raw =
          E.changes ~eq:equal_t2mi_mode_raw
          @@ E.map (fun (x : group) -> x.status.t2mi_mode) group
      ; input
      ; status =
          E.changes ~eq:equal_status
          @@ E.map (fun (x : group) -> x.status.status) group
      ; reset =
          E.map (fun (x : group) ->
              { timestamp = x.status.status.timestamp }) group
      ; errors = hw_errors
      } in
    let ts =
      { info = map ~eq:equal_info ts_info
      ; services = map ~eq:equal_services services
      ; tables = map ~eq:equal_tables tables
      ; pids = map ~eq:equal_pids pids
      ; bitrates
      ; errors = map_errors Events.to_ts_errors group
      } in
    let t2mi =
      { structures = t2mi_info
      ; errors = map_errors Events.to_t2mi_errors group
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
      ; jitter } in
    let (push_events : push_events) =
      { devinfo = set_devinfo
      ; t2mi_mode = set_t2mi_mode
      ; jitter_mode = set_jitter_mode
      ; state = set_state
      ; group = set_group
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

  let create_api sender step_duration
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
        (fun ?section ?table_id_ext ?ext_info_1 ?ext_info_2
             ~id ~table_id () ->
          match find_stream_by_id id @@ React.S.value events.streams with
          | None -> Lwt_result.fail Streams.TS.Stream_not_found
          | Some s ->
             begin match s.orig_id with
             | TS_multi id ->
                let params =
                  { stream_id = id
                  ; table_id
                  ; section
                  ; table_id_ext
                  ; ext_info_1
                  ; ext_info_2
                  } in
                let req = Get_section { request_id = get_id (); params } in
                let timer = Timer.steps ~step_duration 125 in
                enqueue state msgs sender req timer None
             (* XXX maybe other error here *)
             | _ -> Lwt_result.fail Streams.TS.Stream_not_found
             end)
    ; get_t2mi_seq =
        (fun params ->
          let req = Get_t2mi_frame_seq { request_id = get_id (); params } in
          let timer = Timer.steps ~step_duration (params.seconds + 10) in
          enqueue state msgs sender req timer None)
    ; get_devinfo = (fun () -> S.value events.device.info)
    ; config = (fun () -> storage#get)
    },
    msgs,
    imsgs

  let create sources sender (storage : config storage)
        step_duration streams_conv =
    let events, push_events =
      create_events sources storage streams_conv in
    let api, msgs, imsgs =
      create_api sender step_duration storage events push_events in
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
