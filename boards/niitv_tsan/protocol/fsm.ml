open Board_niitv_tsan_types
open Application_types

(* TODO: 1. add structure, bitrate, deverr, jitter, t2mi info probes.
         2. remove unclaimed messages from rsp queue
         3. remove unclaimed message parts from part acc (in protocol) *)

module List = Boards.Util.List

type event =
  [ `Status of Parser.Status.t
  | `Streams of Stream.Multi_TS_ID.t list
  | `Ts_errors of Stream.Multi_TS_ID.t * Error.t list
  | `T2mi_errors of Stream.Multi_TS_ID.t * Error.t list
  | `End_of_errors
  | `End_of_transmission
  ]

type api_msg =
  Request.rsp Lwt_stream.t
  -> event Lwt_stream.t
  -> (unit, Request.error) result Lwt.t

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

let to_raw_stream
    ~input_source
    ~t2mi_source
    (mode : t2mi_mode)
    (input : input)
    (id : Stream.Multi_TS_ID.t) =
  let stream_id = Stream.Multi_TS_ID.stream_id id in
  let src = match Stream.Multi_TS_ID.source_id id, input_of_int stream_id with
    (* Transport stream coming from SPI input. *)
    | src, Some SPI when src = input_source -> `SPI
    (* Transport stream coming from ASI input. *)
    | src, Some ASI when src = input_source -> `ASI
    (* T2-MI PLP. *)
    | src, _ when src = t2mi_source -> `T2MI stream_id
    (* Incoming multi-id transport stream. *)
    | _ -> match input with SPI -> `SPI | ASI -> `ASI in
  let source = match src, mode with
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
      if Stream.Multi_TS_ID.equal id mode.stream && mode.enabled
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

(* FIXME Not applicable for this kind of device *)
let loop (type a)
    (stream : Request.rsp Lwt_stream.t)
    (req : a Request.t) : (a, Request.error) result Lwt.t =
  let rec aux () =
    Lwt_stream.next stream
    >>= fun x ->
    match Parser.is_response req x with
    | None -> aux ()
    | Some (Ok _ as x) -> Lwt.return x
    | Some (Error _ as e) -> Lwt.return e in
  aux ()

let wait_status req events f =
  let rec loop () =
    let events = Lwt_stream.filter_map (function
        | `Status x -> Some x
        | _ -> None)
      @@ Lwt_stream.clone events in
    Lwt_stream.next events
    >>= fun status ->
    if f status
    then Lwt.return_ok ()
    else loop () in
  Lwt.pick [loop (); sleep @@ Request.timeout req]

let request (type a)
    ?(flush = false)
    (src : Logs.src)
    (stream : Request.simple_tag Request.msg Lwt_stream.t)
    (events : event Lwt_stream.t)
    (sender : sender)
    (req : a Request.t) : (a, Request.error) result Lwt.t =
  sender.send req
  >>= (fun () -> if flush then Lwt_stream.junk_old stream else Lwt.return ())
  >>= fun () ->
  let wait_response req =
    Lwt.pick
      [ loop stream req
      ; sleep @@ Request.timeout req ] in
  (match req with
   | Get_devinfo -> wait_response req
   | Get_deverr _ -> wait_response req
   | Get_mode -> wait_response req
   | Get_t2mi_seq _ -> wait_response req
   | Get_section _ -> wait_response req
   | Get_bitrate _ -> wait_response req
   | Get_structure _ -> wait_response req
   | Get_t2mi_info _ -> wait_response req
   | Set_src_id _ -> Lwt.return_ok ()
   | Reset -> wait_status req events (fun status -> status.basic.reset)
   | Set_jitter_mode m ->
     wait_status req events (fun status ->
         equal_jitter_mode status.jitter_mode m)
   | Set_mode m ->
     wait_status req events (fun status ->
         equal_t2mi_mode status.t2mi_mode m.t2mi_mode
         && equal_input status.input m.input))
  >>= function
  | Error e -> log_error src req e; Lwt.return_error e
  | Ok x -> log_ok src req x; Lwt.return_ok x

let start
    (src : Logs.src)
    (sender : sender)
    (req_queue : api_msg Lwt_stream.t)
    (rsp_queue : Request.rsp Lwt_stream.t)
    (evt_queue : Request.evt Lwt_stream.t)
    (kv : config Kv_v.rw)
    (set_state : ?step:React.step -> Topology.state -> unit)
    (set_devinfo : ?step:React.step -> devinfo -> unit)
    (set_status : ?step:React.step -> Parser.Status.t -> unit)
    (set_errors : ?step:React.step -> (Stream.Multi_TS_ID.t * Error.t list) list -> unit)
    (set_streams : ?step:React.step -> Stream.Raw.t list -> unit) =

  let (module Logs : Logs.LOG) = Logs.src_log src in

  let evt_queue = Lwt_stream.filter_map (fun (msg : Request.evt) ->
      let ( >>= ) x f = match x with Ok x -> f x | Error _ as e -> e in
      let res = match msg with
        | { tag = `Status; data } ->
          Parser.Status.parse data
          >>= fun x -> Ok (`Status x)
        | { tag = `Streams; data } ->
          Parser.parse_streams data
          >>= fun x -> Ok (`Streams x)
        | { tag = `Ts_errors; data } ->
          Parser.TS_error.parse data
          >>= fun x -> Ok (`Ts_errors x)
        | { tag = `T2mi_errors; data } ->
          Parser.T2MI_error.parse data
          >>= fun x -> Ok (`T2mi_errors x)
        | { tag = `End_of_errors; _ } -> Ok `End_of_errors
        | { tag = `End_of_transmission; _ } -> Ok `End_of_transmission in
      match res with
      | Ok x -> Some x
      | Error e ->
        Logs.err (fun m ->
            m "Error parsing '%s' event: %s" (Request.event_tag_to_string msg.tag)
            @@ Request.error_to_string e);
        None)
      evt_queue in

  let rec first_step () =
    Logs.info (fun m -> m "Start of connection establishment...");
    Lwt_stream.junk_old req_queue
    >>= fun () -> Lwt_stream.junk_old evt_queue
    >>= fun () -> Lwt_stream.junk_old rsp_queue
    >>= fun () -> set_state `No_response; detect_device ()

  and restart () =
    Logs.info (fun m -> m "Restarting...");
    set_state `No_response;
    Lwt_stream.junk_old req_queue
    >>= fun () -> Lwt_stream.junk_old evt_queue
    >>= fun () -> Lwt_stream.junk_old rsp_queue
    >>= fun () -> Lwt_unix.sleep cooldown_timeout
    >>= first_step

  and detect_device () =
    let rec loop () =
      Lwt.pick
        [ (Lwt_stream.next evt_queue >>= fun x -> Lwt.return @@ `E x)
        ; (Lwt_stream.next rsp_queue >>= fun x -> Lwt.return @@ `R x) ]
      >>= function
      | `E `Status _ ->
        Logs.debug (fun m ->
            m "The device was already initialized, \
               got status event");
        request ~flush:true src rsp_queue evt_queue sender Request.Get_devinfo
      | `R `Simple { tag = `Devinfo; data } ->
        (match Parser.parse_devinfo data with
         | Error _ as e -> Lwt.return e
         | Ok info as x ->
           Logs.debug (fun m ->
               m "The device is waiting, for initialization, \
                  got device info event: %a" pp_devinfo info);
           Lwt.return x)
      | _ -> loop () in
    loop ()
    >>= function
    | Ok x ->
      set_state `Init;
      set_devinfo x;
      Logs.info (fun m -> m "Connection established, \
                             device initialization started...");
      Lwt_stream.junk_old evt_queue
      >>= fun () -> Lwt_stream.junk_old rsp_queue
      >>= init_device
    | Error e ->
      Logs.err (fun m ->
          m "Got error during detect step: %s"
          @@ Request.error_to_string e);
      restart ()

  and init_device () =
    kv#get
    >>= fun { input_source; t2mi_source; input; t2mi_mode; jitter_mode } ->
    sender.send Request.(Set_src_id { input_source; t2mi_source })
    >>= fun () -> sender.send Request.(Set_mode { input; t2mi_mode })
    >>= fun () -> sender.send Request.(Set_jitter_mode jitter_mode)
    >>= fun () ->
    (* Wait for status - make sure that the desired mode is set. *)
    let rec status_loop () =
      Lwt_stream.next evt_queue
      >>= function
      | `Status status ->
        if equal_t2mi_mode status.t2mi_mode t2mi_mode
        && equal_jitter_mode status.jitter_mode jitter_mode
        then Lwt.return_ok ()
        else status_loop ()
      | _ -> status_loop () in
    Lwt.pick [status_loop (); sleep status_timeout]
    >>= function
    | Ok () ->
      set_state `Fine;
      Logs.info (fun m -> m "Initialization done!");
      idle ()
    | Error e ->
      Logs.err (fun m ->
          m "Initialization failed: %s"
          @@ Request.error_to_string e);
      restart ()

  and idle ?timer ?prev () =
    let timer = match timer with
      | Some x -> x
      | None -> Lwt_unix.sleep status_timeout >>= fun () -> Lwt.return `Tm in
    let wait_client =
      Lwt_stream.next req_queue
      >>= fun x -> Lwt.return (`C x) in
    let wait_message =
      Lwt_stream.next evt_queue
      >>= function
      | `Status status -> Lwt.return @@ `S status
      | rsp -> Lwt.return `N in
    Lwt.pick
      [ timer
      ; wait_message
      ; wait_client
      ]
    >>= function
    | `S status ->
      let acc = { prev; status; streams = []; errors = [] } in
      wait_streams acc
    | `N -> idle ?prev ()
    | `C send ->
      (send rsp_queue evt_queue
       >>= function
       | Ok () -> idle ?prev ()
       | Error _ -> idle ?prev ())
    | `E e ->
      Logs.err (fun m ->
          m "Error parsing status: %s"
          @@ Request.error_to_string e);
      restart ()
    | `Tm ->
      Logs.err (fun m ->
          m "Seems that the device is not responding, \
             got no status for %g seconds" status_timeout);
      restart ()

  and wait_streams acc =
    Lwt_stream.next evt_queue
    >>= function
    | `Streams x -> wait_errors { acc with streams = x }
    | event ->
      log_unexpected_event src [`Streams] event;
      restart ()

  and wait_errors acc =
    Lwt_stream.next evt_queue
    >>= function
    | `T2mi_errors (stream, errors) ->
      let errors =
        List.Assoc.update ~eq:(=) (function
            | None -> Some errors
            | Some x -> Some (errors @ x))
          stream acc.errors in
      wait_errors { acc with errors }
    | `Ts_errors (stream, errors) ->
      let errors =
        List.Assoc.update ~eq:(=) (function
            | None -> Some errors
            | Some x -> Some (errors @ x))
          stream acc.errors in
      wait_errors { acc with errors }
    | `End_of_errors -> wait_eot acc
    | `End_of_transmission as rsp ->
      (match acc.errors with
       | [] -> finalize_events acc
       | _ ->
         log_unexpected_event src [`End_of_errors] rsp;
         restart ())
    | rsp ->
      let exp =
        [ `T2mi_errors
        ; `Ts_errors
        ; `End_of_errors
        ; `End_of_transmission
        ] in
      log_unexpected_event src exp rsp; restart ()

  and wait_eot acc =
    Lwt_stream.next evt_queue
    >>= function
    | `End_of_transmission -> finalize_events acc
    | rsp ->
      log_unexpected_event src [`End_of_transmission] rsp;
      restart ()

  and finalize_events { prev; status; streams; errors } =
    let request_structures () =
      let request_id = Serializer.get_request_id () in
      let req = Request.Get_structure { request_id; stream = `All } in
      request ~flush:true src rsp_queue evt_queue sender req in
    (match prev with
     | None -> request_structures ()
     | Some versions ->
       if versions.ts_common <> status.versions.ts_common
       then request_structures ()
       else Lwt.return_ok [])
    >>= function
    | Ok structures ->
      kv#get
      >>= fun { input_source; t2mi_source; _ } ->
      let raw_streams =
        List.filter_map (to_raw_stream
                           ~input_source
                           ~t2mi_source
                           status.t2mi_mode
                           status.input) streams in
      let step = React.Step.create () in
      set_status ~step status;
      set_errors ~step errors;
      set_streams ~step raw_streams;
      React.Step.execute step;
      idle ~prev:status.versions ()
    | Error e ->
      Logs.err (fun m -> m "Error getting structures");
      restart ()

  in
  first_step ()
