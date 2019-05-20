open Board_niitv_tsan_types
open Application_types

(* TODO:
   1. add structure, bitrate, deverr, jitter, t2mi info probes. - DONE
   2. remove unclaimed messages from rsp queue - DONE
   3. remove unclaimed message parts from part acc (in protocol) - DONE
   4. catch devinfo message without corresponding request - MAYBE THIS IS NOT NEEDED AT ALL
   5. implement client request queue - DONE
   6. implement T2-MI monitoring setup after stream appeared/dissapeared - DONE
   7. set equal timestamps in probes
   8. preserve structures/t2mi-info when no corresponding stream is found
   9. implement jitter measurements - NEXT
   10. check device typ & version equality before continue *)

module List = Boards.Util.List

let ( % ) f g x = f (g x)

type event =
  [ `Status of Parser.Status.t
  | `Streams of Stream.Multi_TS_ID.t list
  | `Ts_errors of Stream.Multi_TS_ID.t * Error.t list
  | `T2mi_errors of Stream.Multi_TS_ID.t * Error.t list
  | `End_of_errors
  | `End_of_transmission
  ]

type probe =
  [ `Bitrate of (Stream.Multi_TS_ID.t * Bitrate.t) list
  | `Structure of (Stream.Multi_TS_ID.t * Structure.t) list
  | `T2MI_info of (Stream.Multi_TS_ID.t * (int * T2mi_info.t) list) list
  | `Deverr of Deverr.t list
  ]

type send_client =
  Request.rsp ts React.event
  -> event Lwt_stream.t
  -> unit Lwt.t

type api_msg = int * send_client

type pending = (int * unit Lwt.t) list

type sender = { send : 'a. 'a Request.t -> unit Lwt.t }

type event_acc =
  { prev : Parser.Status.versions option
  ; status : Parser.Status.t
  ; streams : Stream.Multi_TS_ID.t list
  ; errors : (Stream.Multi_TS_ID.t * Error.t list) list
  }

let ( >>= ) = Lwt.( >>= )

let cooldown_timeout = 10.

let status_timeout = 8.

let event_timeout = 3.

let to_raw_stream
    ~input_source
    ~t2mi_source
    (status : Parser.Status.t)
    (id : Stream.Multi_TS_ID.t) =
  let stream_id = Stream.Multi_TS_ID.stream_id id in
  let src = match Stream.Multi_TS_ID.source_id id, input_of_enum stream_id with
    (* Transport stream coming from SPI input. *)
    | src, Some SPI when src = input_source -> `SPI
    (* Transport stream coming from ASI input. *)
    | src, Some ASI when src = input_source -> `ASI
    (* T2-MI PLP. *)
    | src, _ when src = t2mi_source -> `T2MI stream_id
    (* Incoming multi-id transport stream. *)
    | _ -> match status.input with SPI -> `SPI | ASI -> `ASI in
  let source = match src, status.t2mi_mode with
    | `T2MI plp, { stream
                 ; t2mi_stream_id = stream_id
                 ; enabled = true
                 ; _ } ->
      let node = Stream.Raw.(Stream (TS_multi stream)) in
      let info = Stream.Source.T2MI { stream_id; plp } in
      Some { Stream.Raw. node; info }
    | `T2MI _, _ -> None
    | `SPI, _ -> Some { node = Port stream_id; info = SPI }
    | `ASI, _ -> Some { node = Port stream_id; info = ASI }
    | `Unknown, _ -> None in
  match source with
  | None -> None
  | Some source ->
    let (typ : Stream.stream_type) =
      if Stream.Multi_TS_ID.equal id status.t2mi_mode.stream
      && status.t2mi_mode.enabled
      && (match status.t2mi_sync with [] -> false | _ -> true)
      then T2MI else TS in
    Some { Stream.Raw. id = TS_multi id; source; typ }

let log_unexpected_event src expected got =
  let expected =
    String.concat "/"
    @@ List.map Request.simple_tag_to_string expected in
  let got = match got with
    | `Status _ -> "Status"
    | `Streams _ -> "Streams"
    | `Ts_errors _ -> "TS errors"
    | `T2mi_errors _ -> "T2-MI errors"
    | `End_of_errors -> "End of errors"
    | `End_of_transmission -> "End of transmission" in
  Logs.err ~src (fun m -> m "Expected '%s', but got '%s'" expected got)

let log_error (type a) src (req : a Request.t) (error : Request.error) =
  Logs.err ~src (fun m ->
      m "Request \"%s\" failed. Error = %s"
        (Request.to_string req)
        (Request.error_to_string error))

let log_ok (type a) src (req : a Request.t) v =
  Logs.debug ~src (fun m ->
      let base =
        Printf.sprintf "Request \"%s\" succeeded"
          (Request.to_string req) in
      let s = match Request.value_to_string req v with
        | None -> base
        | Some v -> Printf.sprintf "%s. Response = %s" base v in
      m "%s" s)

let sleep timeout =
  Lwt_unix.sleep timeout
  >>= fun () -> Lwt.return_error Request.Timeout

let wait_status req events f =
  let events = Lwt_stream.filter_map (function
      | `Status x -> Some x
      | _ -> None)
    @@ Lwt_stream.clone events in
  let rec loop () =
    Lwt_stream.next events
    >>= fun status ->
    if f status
    then Lwt.return_ok ()
    else loop () in
  Lwt.pick [loop (); sleep @@ Request.timeout req]

let wait_response req (ev : Request.rsp ts React.event) =
  let t, w = Lwt.task () in
  let ev = Util_react.E.map (fun rsp ->
      match Parser.is_response req rsp with
      | None -> ()
      | Some x -> Lwt.wakeup_later w x) ev in
  Lwt.on_cancel t (fun () -> React.E.stop ev);
  Lwt.pick [t; sleep @@ Request.timeout req]

let request (type a)
    (src : Logs.src)
    (stream : Request.rsp ts React.event)
    (events : event Lwt_stream.t)
    (sender : sender)
    (req : a Request.t) : (a, Request.error) result Lwt.t =
  Logs.debug ~src (fun m -> m "Requesting \"%s\"" @@ Request.to_string req);
  sender.send req
  >>= fun () ->
  (match req with
   | Get_devinfo -> wait_response req stream
   | Get_deverr _ -> wait_response req stream
   | Get_mode -> wait_response req stream
   | Get_t2mi_seq _ -> wait_response req stream
   | Get_section _ -> wait_response req stream
   | Get_bitrate _ -> wait_response req stream
   | Get_structure _ -> wait_response req stream
   | Get_t2mi_info _ -> wait_response req stream
   | Set_src_id _ -> Lwt.return_ok ()
   | Reset -> wait_status req events (fun status -> status.basic.reset)
   | Set_jitter_mode m ->
     wait_status req events (fun status ->
         equal_jitter_mode status.jitter_mode m)
   | Set_mode m ->
     wait_status req events (fun status ->
         (* equal_t2mi_mode ~with_stream_id:false status.t2mi_mode m.t2mi_mode
          * && *) equal_input status.input m.input))
  >>= function
  | Error e -> log_error src req e; Lwt.return_error e
  | Ok x -> log_ok src req x; Lwt.return_ok x

type 'a set = ?step:React.step -> 'a -> unit

let start
    (src : Logs.src)
    (sender : sender)
    (pending : pending ref)
    (req_queue : api_msg Queue_lwt.t)
    (rsp_event : Request.rsp ts React.event)
    (evt_queue : Request.evt ts Lwt_stream.t)
    (kv : config Kv_v.rw)
    (t2mi_mode_listener : t2mi_mode Lwt_stream.t)
    (set_state : Topology.state set)
    (set_devinfo : devinfo set)
    (set_status : Parser.Status.t set)
    (set_errors : (Stream.Multi_TS_ID.t * Error.t list) list set)
    (set_streams : Stream.Raw.t list set)
    (set_structure : (Stream.Multi_TS_ID.t * Structure.t) list set)
    (set_bitrate : (Stream.Multi_TS_ID.t * Bitrate.t) list set)
    (set_t2mi_info : (Stream.Multi_TS_ID.t * (int * T2mi_info.t) list) list set)
    (set_deverr : Deverr.t list set) =

  let (module Logs : Logs.LOG) = Logs.src_log src in

  let clear_pending () =
    List.iter (Lwt.cancel % snd) !pending;
    pending := [] in

  let reset_notifs () =
    let step = React.Step.create () in
    set_state ~step `No_response;
    set_streams ~step [];
    set_structure ~step [];
    set_t2mi_info ~step [];
    React.Step.execute step in

  let evt_queue = Lwt_stream.filter_map (fun { data; timestamp } ->
      let ( >>= ) x f = match x with Ok x -> f x | Error _ as e -> e in
      let res = match (data : Request.evt) with
        | { tag = `Status; body } ->
          Parser.Status.parse ~timestamp body
          >>= fun x -> Ok (`Status x)
        | { tag = `Streams; body } ->
          Parser.parse_streams body
          >>= fun x -> Ok (`Streams x)
        | { tag = `Ts_errors; body } ->
          Parser.TS_error.parse ~timestamp body
          >>= fun x -> Ok (`Ts_errors x)
        | { tag = `T2mi_errors; body } ->
          Parser.T2MI_error.parse ~timestamp body
          >>= fun x -> Ok (`T2mi_errors x)
        | { tag = `End_of_errors; _ } -> Ok `End_of_errors
        | { tag = `End_of_transmission; _ } -> Ok `End_of_transmission in
      match res with
      | Ok x -> Some x
      | Error e ->
        Logs.err (fun m ->
            m "Error parsing '%s' event: %s" (Request.event_tag_to_string data.tag)
            @@ Request.error_to_string e);
        None)
      evt_queue in

  let rec restart () =
    Logs.info (fun m -> m "Restarting...");
    clear_pending ();
    reset_notifs ();
    Queue_lwt.clear req_queue
    >>= fun () -> Lwt_stream.junk_old evt_queue
    >>= fun () -> Lwt_stream.junk_old t2mi_mode_listener
    >>= fun () -> Lwt_unix.sleep cooldown_timeout
    >>= detect

  and detect () =
    Logs.info (fun m -> m "Start of connection establishment...");
    set_state `Detect;
    let rec loop () =
      Lwt.pick
        [ (Lwt_stream.next evt_queue >>= fun x -> Lwt.return @@ `E x)
        ; (Util_react.E.next rsp_event >>= fun x -> Lwt.return @@ `R x) ]
      >>= function
      | `E `Status _ ->
        Logs.debug (fun m ->
            m "The device was already initialized, \
               got status event");
        request src rsp_event evt_queue sender Request.Get_devinfo
      | `R { data = `Simple { tag = `Devinfo; body }; _ } ->
        (match Parser.parse_devinfo body with
         | Error _ as e -> Lwt.return e
         | Ok info as x ->
           Logs.debug (fun m ->
               m "The device is waiting, for initialization, \
                  got device info event: %a" pp_devinfo info);
           Lwt.return x)
      | _ -> loop () in
    loop ()
    >>= function
    | Error e -> Logs.err (fun m ->
        m "Got error during detect step: %s"
        @@ Request.error_to_string e);
      restart ()
    | Ok x ->
      set_state `Init;
      set_devinfo x;
      Logs.info (fun m -> m "Connection established, \
                             device initialization started...");
      Lwt_stream.junk_old evt_queue
      >>= initialize

  and initialize () =
    kv#get
    >>= fun { input_source; t2mi_source; input; t2mi_mode; jitter_mode } ->
    sender.send Request.(Set_src_id { input_source; t2mi_source })
    >>= fun () ->
    let t2mi_mode = match t2mi_mode.stream_id with
      | None -> t2mi_mode
      | Some _ -> { t2mi_mode with enabled = false } in
    sender.send Request.(Set_mode { input; t2mi_mode })
    >>= fun () -> sender.send Request.(Set_jitter_mode jitter_mode)
    >>= fun () ->
    (* Wait for status - make sure that the desired mode is set. *)
    let rec wait_status () =
      Lwt_stream.next evt_queue
      >>= function
      | `Status status ->
        if equal_t2mi_mode ~with_stream_id:false status.t2mi_mode t2mi_mode
        && equal_jitter_mode status.jitter_mode jitter_mode
        then Lwt.return_ok ()
        else wait_status ()
      | _ -> wait_status () in
    Lwt.pick [wait_status (); sleep status_timeout]
    >>= function
    | Ok () -> idle ()
    | Error e ->
      Logs.err (fun m ->
          m "Initialization failed: %s"
          @@ Request.error_to_string e);
      restart ()

  and idle () =
    set_state `Fine;
    Logs.info (fun m -> m "Initialization done!");
    Lwt.pick
      [ status_loop ()
      ; t2mi_loop ()
      ; client_loop () ]
    >>= restart

  and t2mi_loop () : unit Lwt.t =
    Lwt_stream.last_new t2mi_mode_listener
    >>= fun t2mi_mode -> kv#get
    >>= fun { input; _ } ->
    let req = Request.Set_mode { input; t2mi_mode } in
    request src rsp_event evt_queue sender req
    >>= function
    | Ok () -> t2mi_loop ()
    | Error e ->
      Logs.err (fun m ->
          m "Error during internal T2-MI mode setup: %s"
          @@ Request.error_to_string e);
      Lwt.return ()

  and client_loop () : unit Lwt.t =
    Lwt.pick
      [ (Queue_lwt.next req_queue >>= fun x -> Lwt.return (`C x))
      ; (Lwt.nchoose @@ List.map snd !pending >>= fun _ -> Lwt.return `R) ]
    >>= function
    | `R ->
      pending := List.filter (Lwt.is_sleeping % snd) !pending;
      Queue_lwt.check_condition req_queue
      >>= client_loop
    | `C (id, send) ->
      let r = send rsp_event evt_queue in
      pending := (id, r) :: !pending;
      client_loop ()

  and status_loop
      ?(timer = Lwt_unix.sleep status_timeout >>= fun () -> Lwt.return `Tm)
      ?(prev : Parser.Status.versions option)
      () : unit Lwt.t =
    let rec wait_status () =
      Lwt_stream.next evt_queue
      >>= function
      | `Status status -> Lwt.return @@ `S status
      | _ -> wait_status () in
    Lwt.pick [timer; wait_status ()]
    >>= function
    | `S status ->
      (let timer = Lwt_unix.sleep status_timeout >>= fun () -> Lwt.return `Tm in
       wait_streams { prev; status; streams = []; errors = [] }
       >>= function
       | Error _ -> Lwt.return ()
       | Ok prev -> status_loop ~timer ~prev ())
    | `Tm ->
      Logs.err (fun m ->
          m "Seems that the device is not responding, \
             got no status for %g seconds" status_timeout);
      Lwt.return ()

  and wait_streams acc =
    Lwt.pick
      [ sleep event_timeout
      ; (Lwt_stream.next evt_queue
         >>= function
         | `Streams x -> Lwt.return_ok { acc with streams = x }
         | event ->
           log_unexpected_event src [`Streams] event;
           Lwt.return_error (Request.Custom "Unexpected event")) ]
    >>= function
    | Ok acc -> wait_errors acc
    | Error e -> Logs.err (fun m ->
        m "Error while waiting for streams event: %s"
        @@ Request.error_to_string e);
      Lwt.return_error e

  and wait_errors acc =
    let wait_event =
      Lwt_stream.next evt_queue
      >>= function
      | `T2mi_errors (stream, errors) ->
        let errors =
          List.Assoc.update ~eq:(=) (function
              | None -> (match errors with [] -> None | l -> Some l)
              | Some x -> Some (errors @ x))
            stream acc.errors in
        Lwt.return_ok (`E { acc with errors })
      | `Ts_errors (stream, errors) ->
        let errors =
          List.Assoc.update ~eq:(=) (function
              | None -> (match errors with [] -> None | l -> Some l)
              | Some x -> Some (errors @ x))
            stream acc.errors in
        Lwt.return_ok (`E { acc with errors })
      | `End_of_errors -> Lwt.return_ok `EOE
      | `End_of_transmission as rsp ->
        (match acc.errors with
         | [] -> Lwt.return_ok `EOT
         | _ ->
           log_unexpected_event src [`End_of_errors] rsp;
           Lwt.return_error (Request.Custom "Unexpected event"))
      | rsp ->
        let exp =
          [ `T2mi_errors
          ; `Ts_errors
          ; `End_of_errors
          ; `End_of_transmission
          ] in
        log_unexpected_event src exp rsp;
        Lwt.return_error (Request.Custom "Unexpected event") in
    Lwt.pick [sleep event_timeout; wait_event]
    >>= function
    | Ok `EOE -> wait_eot acc
    | Ok `EOT -> finalize_events acc
    | Ok `E acc -> wait_errors acc
    | Error e -> Logs.err (fun m ->
        m "Error while waiting for errors event: %s"
        @@ Request.error_to_string e);
      Lwt.return_error e

  and wait_eot acc =
    Lwt.pick
      [ sleep event_timeout
      ; (Lwt_stream.next evt_queue
         >>= function
         | `End_of_transmission -> Lwt.return_ok ()
         | rsp ->
           log_unexpected_event src [`End_of_transmission] rsp;
           Lwt.return_error (Request.Custom "Unexpected event")) ]
    >>= function
    | Ok () -> finalize_events acc
    | Error e -> Logs.err (fun m ->
        m "Error while waiting for EOT event: %s"
        @@ Request.error_to_string e);
      Lwt.return_error e

  and get_structures
      ~(old : Parser.Status.versions option)
      ~(cur : Parser.Status.versions)
      (probes : probe list) =
    let fetch = match old with
      | None -> true
      | Some { ts_common; _ } -> ts_common <> cur.ts_common in
    if not fetch then Lwt.return_ok probes
    else
      let request_id = Serializer.get_request_id () in
      let req = Request.Get_structure { request_id; stream = `All } in
      request src rsp_event evt_queue sender req
      >>= function
      | Error _ as e -> Lwt.return e
      | Ok x -> Lwt.return_ok (`Structure x :: probes)

  and get_bitrate has_sync probes =
    if not has_sync
    then Lwt.return_ok (`Bitrate [] :: probes)
    else
      let request_id = Serializer.get_request_id () in
      let req = Request.Get_bitrate request_id in
      request src rsp_event evt_queue sender req
      >>= function
      | Error _ as e -> Lwt.return e
      | Ok x -> Lwt.return_ok (`Bitrate x :: probes)

  and get_t2mi_info
      (old : Parser.Status.versions option)
      (cur : Parser.Status.t)
      (probes : probe list) =
    let ids = match cur.t2mi_sync, old with
      | [], _ -> []
      | sync, None -> sync
      | sync, Some prev ->
        List.fold_left (fun acc i ->
            match List.assoc_opt i cur.versions.t2mi,
                  List.assoc_opt i prev.t2mi with
            | None, _ | _, None -> acc
            | Some c, Some o -> if c <> o then i :: acc else acc) [] sync in
    match ids with
    | [] -> Lwt.return_ok probes
    | ids ->
      let rec request_loop probes = function
        | [] -> Lwt.return_ok probes
        | t2mi_stream_id :: tl ->
          let request_id = Serializer.get_request_id () in
          let req = Request.Get_t2mi_info { request_id; t2mi_stream_id } in
          request src rsp_event evt_queue sender req
          >>= function
          | Ok x -> request_loop (x :: probes) tl
          | Error e -> Lwt.return_error e in
      Lwt_result.Infix.(
        request_loop [] ids
        >>= fun x ->
        let stream = cur.t2mi_mode.stream in
        Lwt.return_ok (`T2MI_info [stream, x] :: probes))

  (* TODO state machine should not depend on the result of this request,
     if it fails - ok, if it returns value after some amount of time - also ok *)
  and get_deverr has_errors probes =
    if not has_errors then Lwt.return_ok probes
    else
      let request_id = Serializer.get_request_id () in
      let req = Request.Get_deverr { request_id; timeout = None } in
      request src rsp_event evt_queue sender req
      >>= function
      | Error _ as e -> Lwt.return e
      | Ok x -> Lwt.return_ok (`Deverr x :: probes)

  and get_probes { prev; status; _ } =
    Lwt_result.Infix.(
      get_deverr status.errors []
      >>= get_structures ~old:prev ~cur:status.versions
      >>= get_bitrate status.basic.has_sync
      >>= get_t2mi_info prev status)

  and finalize_events
      ({ prev; status; streams; errors } as acc) =
    let rec wait_status () =
      Lwt_stream.next evt_queue
      >>= function
      | `Status status ->
        Lwt.return_error (Request.Custom "Got status earlier than probes")
      | _ -> wait_status () in
    Lwt.pick [get_probes acc; wait_status ()]
    >>= function
    | Error e -> Logs.err (fun m ->
        m "Error while fetching probes: %s"
        @@ Request.error_to_string e);
      Lwt.return_error e
    | Ok probes ->
      kv#get
      >>= fun { input_source; t2mi_source; _ } ->
      let raw_streams =
        List.filter_map (to_raw_stream
                           ~input_source
                           ~t2mi_source
                           status) streams in
      let step = React.Step.create () in
      set_status ~step status;
      set_streams ~step raw_streams;
      (match errors with
       | [] -> ()
       | errors -> set_errors ~step errors);
      List.iter (function
          | `Structure x -> set_structure ~step x
          | `Bitrate x -> set_bitrate ~step x
          | `T2MI_info x -> set_t2mi_info ~step x
          | `Deverr x -> set_deverr ~step x) probes;
      React.Step.execute step;
      Lwt.return_ok status.versions

  in
  detect
