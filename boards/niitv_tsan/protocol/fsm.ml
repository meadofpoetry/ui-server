open Board_niitv_tsan_types
open Application_types

module List = Boards.Util.List

type api_msg = (Request.rsp_tag Request.msg Lwt_stream.t -> unit Lwt.t)

type sender = { send : 'a. 'a Request.t -> unit Lwt.t }

type event_acc =
  { prev : Parser.Status.versions option
  ; status : Parser.Status.t
  ; streams : Stream.Multi_TS_ID.t list
  ; errors : (Stream.Multi_TS_ID.t * Error.t list) list
  }

module Await_queue = struct

  type pending =
    { resolve : Request.rsp -> bool
    }

  type t = pending list

  let resolve (t : t) (response : Request.rsp) =
    let rec aux acc = function
      | [] -> acc
      | hd :: tl ->
        if hd.resolve response
        then acc @ tl
        else aux (hd :: acc) tl in
    aux t

end

let ( >>= ) = Lwt.( >>= )

let cooldown_timeout = 10.

let status_timeout = 8.

let to_raw_stream
    ~input_source
    ~t2mi_source
    (mode : t2mi_mode)
    (input : input)
    (id : Stream.Multi_TS_ID.t) =
  let open Stream.Source in
  let open Stream.Raw in
  let source_id = Stream.Multi_TS_ID.source_id id in
  let stream_id = Stream.Multi_TS_ID.stream_id id in
  let src = match source_id, input_of_int stream_id with
    | src, Some SPI when src = input_source -> `Spi
    | src, Some ASI when src = input_source -> `Asi
    | src, _ when src = t2mi_source -> `T2mi stream_id
    | _ -> match input with SPI -> `Spi | ASI -> `Asi in
  let source = match src, mode with
    | `T2mi plp, { stream
                 ; t2mi_stream_id = stream_id
                 ; enabled = true
                 ; _ } ->
      let node = Stream (TS_multi stream) in
      let info = T2MI { stream_id; plp } in
      Some { node; info }
    | `T2mi _, _ -> None
    | `Spi, _ -> Some { node = Port stream_id; info = SPI }
    | `Asi, _ -> Some { node = Port stream_id; info = ASI }
    | `Unknown, _ -> None in
  match source with
  | None -> None
  | Some source ->
    let (typ : Stream.stream_type) =
      if Stream.Multi_TS_ID.equal id mode.stream && mode.enabled
      then T2MI else TS in
    Some { id = TS_multi id; source; typ }

let log_unexpected_message src expected (rsp : Request.rsp) =
  let expected =
    String.concat "/"
    @@ List.map Request.rsp_tag_to_string expected in
  let got = match rsp with
    | `Simple { tag; _ } -> Request.rsp_tag_to_string tag
    | `Complex { tag; _ } -> Request.complex_tag_to_string tag in
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

let request (type a)
    ?(flush = false)
    (src : Logs.src)
    (stream : Request.rsp_tag Request.msg Lwt_stream.t)
    (sender : sender)
    (req : a Request.t) : (a, Request.error) result Lwt.t =
  sender.send req
  >>= (fun () -> if flush then Lwt_stream.junk_old stream else Lwt.return ())
  >>= fun () ->
  let wait req = Lwt.pick [loop stream req; sleep @@ Request.timeout req] in
  (match req with
   | Get_devinfo -> wait req
   | Get_deverr _ -> wait req
   | Get_mode -> wait req
   | Get_t2mi_seq _ -> wait req
   | Get_section _ -> wait req
   | Get_bitrate _ -> wait req
   | Get_structure _ -> wait req
   | Get_t2mi_info _ -> wait req
   | Set_mode _ -> Lwt.return_ok ()
   | Set_jitter_mode _ -> Lwt.return_ok ()
   | Reset -> Lwt.return_ok ()
   | Set_src_id _ -> Lwt.return_ok ())
  >>= function
  | Error e -> log_error src req e; Lwt.return_error e
  | Ok x -> log_ok src req x; Lwt.return_ok x

let start
    (src : Logs.src)
    (sender : sender)
    (req_queue : api_msg Lwt_stream.t)
    (rsp_queue : Request.rsp Lwt_stream.t)
    (kv : config Kv_v.rw)
    (set_state : ?step:React.step -> Topology.state -> unit)
    (set_devinfo : ?step:React.step -> devinfo -> unit)
    (set_status : ?step:React.step -> Parser.Status.t -> unit)
    (set_streams : ?step:React.step -> Stream.Raw.t list -> unit) =

  let (module Logs : Logs.LOG) = Logs.src_log src in

  let rec first_step () =
    Logs.info (fun m -> m "Start of connection establishment...");
    Lwt_stream.junk_old req_queue
    >>= fun () -> Lwt_stream.junk_old rsp_queue
    >>= fun () -> set_state `No_response; detect_device ()

  and restart () =
    Logs.info (fun m -> m "Restarting...");
    set_state `No_response;
    Lwt_stream.junk_old req_queue
    >>= fun () -> Lwt_stream.junk_old rsp_queue
    >>= fun () -> Lwt_unix.sleep cooldown_timeout
    >>= first_step

  and detect_device () =
    let rec loop () =
      Lwt_stream.next rsp_queue
      >>= function
      | `Simple { tag = `Status; _ } ->
        Logs.debug (fun m ->
            m "The device was already initialized, \
               got status event");
        request ~flush:true src rsp_queue sender Request.Get_devinfo
      | `Simple { tag = `Devinfo; data } ->
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
    | Ok x -> init_device x
    | Error e ->
      Logs.err (fun m ->
          m "Got error during detect step: %s"
          @@ Request.error_to_string e);
      restart ()

  and init_device (info : devinfo) =
    set_state `Init;
    set_devinfo info;
    Logs.info (fun m -> m "Connection established, device initialization started...");
    kv#get
    >>= fun { input_source
            ; t2mi_source
            ; input
            ; t2mi_mode
            ; jitter_mode
            } ->
    Lwt_result.Infix.(
      let req = Request.Set_src_id { input_source; t2mi_source } in
      request ~flush:true src rsp_queue sender req
      >>= fun () ->
      let req = Request.Set_mode { input; t2mi_mode } in
      request ~flush:true src rsp_queue sender req
      >>= fun () ->
      let req = Request.Set_jitter_mode jitter_mode in
      request ~flush:true src rsp_queue sender req
      >>= fun () -> Lwt.return_ok ())
    >>= function
    | Ok () ->
      Lwt_stream.junk_old rsp_queue
      >>= fun () ->
      set_state `Fine;
      Logs.info (fun m -> m "Initialization done!");
      idle ()
    | Error e ->
      Logs.err (fun m ->
          m "Got error during init step: %s"
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
      Lwt_stream.next rsp_queue
      >>= function
      | `Simple { tag = `Status; data } ->
        Lwt.return
        @@ (match Parser.Status.parse data with
            | Ok status -> `S status
            | Error e -> `E e)
      | rsp -> Lwt.return (`R rsp) in
    (* TODO we should not wait for status forever, there must be a timeout *)
    Lwt.pick
      [ timer
      ; wait_message
      ; wait_client
      ]
    >>= function
    | `S status ->
      print_endline "Got status";
      let acc = { prev; status; streams = []; errors = [] } in
      wait_streams acc
    | `R _ -> idle ?prev ()
    | `C send -> send rsp_queue >>= idle ?prev
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
    Lwt_stream.next rsp_queue
    >>= function
    | `Simple { tag = `Streams; data } ->
      (match Parser.parse_streams data with
       | Ok streams -> wait_errors { acc with streams }
       | Error e ->
         Logs.err (fun m ->
             m "Failure parsing streams: %s"
             @@ Request.error_to_string e);
         restart ())
    | rsp ->
      log_unexpected_message src [`Streams] rsp;
      restart ()

  and wait_errors acc =
    Lwt_stream.next rsp_queue
    >>= function
    | `Simple { tag = `T2mi_errors; data } ->
      let acc = match Parser.T2MI_error.parse data with
        | Ok (stream, errors) ->
          let errors =
            List.Assoc.update ~eq:(=) (function
                | None -> Some errors
                | Some x -> Some (errors @ x))
              stream acc.errors in
          { acc with errors }
        | Error e ->
          Logs.err (fun m ->
              m "Failure parsing T2-MI errors: %s"
              @@ Request.error_to_string e);
          acc in
      wait_errors acc
    | `Simple { tag = `Ts_errors; data } ->
      let acc = match Parser.TS_error.parse data with
        | Ok (stream, errors) ->
          let errors =
            List.Assoc.update ~eq:(=) (function
                | None -> Some errors
                | Some x -> Some (errors @ x))
              stream acc.errors in
          { acc with errors }
        | Error e ->
          Logs.err (fun m ->
              m "Failure parsing TS errors: %s"
              @@ Request.error_to_string e);
          acc in
      wait_errors acc
    | `Simple { tag = `End_of_errors; _ } -> wait_eot acc
    | `Simple { tag = `End_of_transmission; _ } as rsp ->
      (match acc.errors with
       | [] -> finalize_events acc
       | _ ->
         log_unexpected_message src [`End_of_errors] rsp;
         restart ())
    | rsp ->
      let exp =
        [ `T2mi_errors
        ; `Ts_errors
        ; `End_of_errors
        ; `End_of_transmission
        ] in
      log_unexpected_message src exp rsp; restart ()

  and wait_eot acc =
    Lwt_stream.next rsp_queue
    >>= function
    | `Simple { tag = `End_of_transmission; _ } -> finalize_events acc
    | rsp ->
      log_unexpected_message src [`End_of_transmission] rsp;
      restart ()

  and finalize_events { prev; status; streams; errors } =
    (* XXX Maybe send probes here?
       Seems that we must send them without waiting for response,
       just putting the to the await queue.
       Do the actions (updating the event) when the thread is
       resolved.
       The only request we should wait for is structure,
       as errors corresponds to pids.
    *)
    let request_structures () =
      print_endline "Requesting structures!";
      let request_id = Serializer.get_request_id () in
      let req = Request.Get_structure { request_id; stream = `All } in
      request ~flush:true src rsp_queue sender req in
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
      (try
         set_status ~step status;
         set_streams ~step raw_streams;
         React.Step.execute step;
         idle ~prev:status.versions ()
       with _ -> Lwt.fail_with "notifs update failure")
    | Error e ->
      Logs.err (fun m -> m "Error getting structures");
      restart ()

  in
  first_step ()
