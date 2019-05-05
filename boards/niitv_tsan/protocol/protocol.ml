open Board_niitv_tsan_types

let status_timeout = 8. (* seconds *)

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

let merge ~(eq:'b -> 'b -> bool)
    (f : 'a -> 'b)
    (streams : Stream.t list React.signal)
    (data : (Multi_TS_ID.t, 'a) List.Assoc.t React.signal)
  : (Stream.ID.t, 'b) List.Assoc.t React.signal =
  let eq = List.equal (Pair.equal Stream.ID.equal eq) in
  React.S.l2 ~eq (fun streams data ->
      List.filter_map (fun (id, x) ->
          match Stream.find_by_multi_id id streams with
          | None -> None
          | Some s -> Some (s.id, f x)) data)
    streams data

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
    let streams = streams_conv raw_streams in
    let structures, set_structures =
      S.create ~eq:(Equal.list (Equal.pair Multi_TS_ID.equal equal_structure)) [] in
    let map_struct ~eq f =
      let eq = Time.equal_timestamped eq in
      merge ~eq (fun (x : structure) -> Time.stamp x.time (f x))
        streams structures in
    let ts_info =
      map_struct ~eq:Ts_info.equal (fun x -> x.info) in
    let tables =
      let eq = List.equal SI_PSI_table.equal in
      map_struct ~eq (fun x -> x.tables) in
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
    let sections =
      S.map ~eq:equal_sections
        (List.map (fun (id, Time.{ timestamp; data }) ->
             id, Time.{ timestamp; data = to_sections data })) tables in
    let device =
      { state; input; info = devinfo; t2mi_mode; jitter_mode; config;
        status; errors = hw_errors } in
    let ts =
      { info = ts_info
      ; services =
          (let eq = List.equal Service.equal in
           map_struct ~eq (fun x -> x.services))
      ; sections
      ; tables
      ; pids =
          (let eq = List.equal Pid.equal in
           map_struct ~eq (fun x -> x.pids))
      ; bitrates
      ; errors = ts_errors
      } in
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
        bitrates = set_bitrates; t2mi_info = set_t2mi_info;
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
                let request_id = Board_serializer.get_request_id () in
                let req = Get_section { request_id; params } in
                let timer = Timer.steps ~step_duration 125. in
                Serializer.enqueue Parser.is_response state msgs sender req timer None
              (* XXX maybe other error here *)
              | _ -> Lwt_result.fail Stream_not_found
            end)
    ; get_t2mi_seq =
        (fun params ->
           let request_id = Board_serializer.get_request_id () in
           let req = Get_t2mi_frame_seq { request_id; params } in
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
