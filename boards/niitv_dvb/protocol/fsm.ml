open Board_niitv_dvb_types

(* TODO maybe PLP list & params should be fetched
   after 'lock' change in measures probes. *)

let ( >>= ) = Lwt.( >>= )

let cooldown_timeout = 10. (* seconds *)

let meas_interval = 1 (* seconds *)

let params_interval = 5 (* seconds *)

let ack_timeout = 2. (* seconds *)

type api_msg = Request.msg Lwt_stream.t -> unit Lwt.t

type 'a set = ?step:React.step -> 'a -> unit

let rec gcd a b =
  if b = 0 then a else gcd b (a mod b)

let log_ok (type a) src (req : a Request.t) v =
  Logs.debug ~src (fun m ->
      m "Request \"%s\" succeeded. Response = %s"
        (Request.to_string req) (Request.value_to_string req v))

let log_error (type a) src (req : a Request.t) (error : Request.error) =
  Logs.err ~src (fun m ->
      m "Request \"%s\" failed. Error = %s"
        (Request.to_string req) (Request.error_to_string error))

let sleep timeout =
  Lwt_unix.sleep timeout
  >>= fun () -> Lwt.return_error Request.Timeout

let rec wait_ack stream =
  Lwt.pick
    [ (Lwt_stream.next stream >>= Lwt.return_ok)
    ; sleep ack_timeout ]
  >>= function
  | Error _ as e -> Lwt.return e
  | Ok ({ tag = `Ack; _ } : Request.msg) -> Lwt.return_ok ()
  | Ok _ -> wait_ack stream

let wait_rsp (type a) stream (req : a Request.t) () =
  let rec loop () =
    Lwt_stream.next stream
    >>= fun x ->
    match Parser.is_response req x with
    | None -> loop ()
    | Some (Ok x) -> Lwt.return_ok x
    | Some (Error e) -> Lwt.return_error e in
  Lwt.pick [loop (); sleep (Request.timeout req)]

let request (type a)
    (src : Logs.src)
    (stream : Request.msg Lwt_stream.t)
    (sender : Cstruct.t -> unit Lwt.t)
    (req : a Request.t) : (a, Request.error) result Lwt.t =
  let rec loop = function
    | 0 -> Lwt.return_error Request.Not_responding
    | n ->
      sender @@ Serializer.serialize req
      >>= fun () -> Lwt_result.Infix.(wait_ack stream >>= wait_rsp stream req)
      >>= function
      | Ok x -> log_ok src req x; Lwt.return_ok x
      | Error e -> log_error src req e; loop (pred n) in
  Lwt_stream.junk_old stream
  >>= fun () -> loop 3

let start (src : Logs.src)
    (sender : Cstruct.t -> unit Lwt.t)
    (req_queue : api_msg Lwt_stream.t)
    (rsp_queue : Request.msg Lwt_stream.t)
    (kv : Device.config Kv_v.rw)
    (set_state : Application_types.Topology.state set)
    (set_devinfo : Device.info set)
    (set_measures : (int * Measure.t ts) list set)
    (set_params : (int * Params.t ts) list set)
    (set_plps : (int * Plp_list.t ts) list set) =

  let (module Logs : Logs.LOG) = Logs.src_log src in

  let wait_probes =
    let cnt_meas = ref 0 in
    let cnt_params = ref 0 in
    let gcd = gcd meas_interval params_interval in
    let rec f () =
      Lwt_unix.sleep @@ float_of_int gcd
      >>= fun () ->
      cnt_meas := !cnt_meas + gcd;
      cnt_params := !cnt_params + gcd;
      if !cnt_params = params_interval
      && !cnt_meas = meas_interval
      then (cnt_params := 0; cnt_meas := 0; Lwt.return `EBOTH)
      else if !cnt_meas mod meas_interval = 0
      then (cnt_meas := 0; Lwt.return `EMEAS)
      else if !cnt_params mod params_interval = 0
      then (cnt_params := 0; Lwt.return `EPARAMS)
      else f () in
    f in

  let rec restart () =
    Logs.info (fun m -> m "Restarting...");
    set_state `No_response;
    Lwt_stream.junk_old rsp_queue
    >>= fun () -> Lwt_stream.junk_old req_queue
    >>= fun () -> Lwt_unix.sleep cooldown_timeout
    >>= detect

  and detect () =
    Logs.info (fun m -> m "Start of connection establishment...");
    set_state `Detect;
    let req = Request.Get_devinfo in
    request src rsp_queue sender req
    >>= function
    | Error e -> Logs.err (fun m ->
        m "Error during detect step: %s"
        @@ Request.error_to_string e);
      restart ()
    | Ok devinfo ->
      let step = React.Step.create () in
      set_state ~step `Init;
      set_devinfo ~step devinfo;
      React.Step.execute step;
      Logs.info (fun m ->
          m "Connection established, device initialization started...");
      set_source_id devinfo.receivers

  and set_source_id tuners =
    kv#get
    >>= fun config ->
    request src rsp_queue sender (Request.Set_src_id config.source)
    >>= function
    | Error _ -> restart ()
    | Ok id ->
      if id = config.source
      then initialize tuners config
      else (
        Logs.err (fun m ->
            m "Failure setting up source ID = %d. \
               Device returned ID = %d" config.source id);
        restart ())

  and initialize tuners (config : Device.config) =
    let rec aux acc = function
      | [] -> Lwt.return_ok acc
      | ((id, _) as hd) :: tl ->
        if not @@ List.mem id tuners
        then (
          Logs.warn (fun m ->
              m "Found configuration to be set for tuner %d, \
                 but the correponding hardware is not present" id);
          aux acc tl)
        else (
          let req = Request.Set_mode hd in
          request src rsp_queue sender req
          >>= function
          | Error e -> Lwt.return_error e
          | Ok x -> aux (x :: acc) tl) in
    aux [] config.mode
    >>= function
    | Error _ -> restart ()
    | Ok x ->
      let mode = List.map (fun (id, ({ mode; _ } : Device.mode_rsp)) ->
          id, mode) x in
      kv#set { config with mode }
      >>= fun () ->
      Logs.info (fun m -> m "Initialization done!");
      set_state `Fine;
      idle tuners ()

  and idle ?timer tuners () =
    let timer = match timer with
      | Some x -> x
      | None -> wait_probes () in
    let wait_msg =
      Lwt_stream.next req_queue
      >>= fun x -> Lwt.return @@ `M x in
    Lwt.choose [wait_msg; timer]
    >>= function
    | `M send -> send rsp_queue >>= idle ~timer tuners
    | (`EMEAS | `EPARAMS | `EBOTH) as probes ->
      Lwt.cancel wait_msg;
      let t = Lwt_result.Infix.(
          match probes with
          | `EMEAS ->
            (pull_measurements tuners
             >>= fun x -> set_measures x; Lwt.return_ok ())
          | `EPARAMS ->
            (pull_parameters tuners
             >>= fun prms -> pull_plps tuners
             >>= fun plps ->
             let step = React.Step.create () in
             set_params ~step prms;
             set_plps ~step plps;
             React.Step.execute step;
             Lwt.return_ok ())
          | `EBOTH ->
            (pull_measurements tuners
             >>= fun meas -> pull_parameters tuners
             >>= fun prms -> pull_plps tuners
             >>= fun plps ->
             let step = React.Step.create () in
             set_params ~step prms;
             set_measures ~step meas;
             set_plps ~step plps;
             React.Step.execute step;
             Lwt.return_ok ()))
      in
      t >>= function Ok () -> idle tuners () | Error _ -> restart ()

  and pull_measurements tuners =
    let rec aux acc = function
      | [] -> Lwt.return_ok acc
      | id :: tl ->
        request src rsp_queue sender (Request.Get_measure id)
        >>= function
        | Error _ as e -> Lwt.return e
        | Ok x -> aux (x :: acc) tl
    in
    aux [] tuners

  and pull_parameters tuners =
    let rec aux acc = function
      | [] -> Lwt.return_ok acc
      | id :: tl ->
        kv#get
        >>= fun config ->
        match List.find_opt (fun (id', _) -> id = id') config.mode with
        | None ->
          Logs.debug (fun m ->
              m "Pull parameters: configuration not found for tuner %d" id);
          aux acc tl
        | Some (id, mode) ->
          match mode.standard with
          (* We need to pull parameters only if current standard is DVB-T2. *)
          | T | C -> aux acc tl
          | T2 ->
            request src rsp_queue sender (Request.Get_params id)
            >>= function
            | Error _ as e -> Lwt.return e
            | Ok x -> aux (x :: acc) tl
    in
    aux [] tuners

  and pull_plps tuners =
    let rec aux acc = function
      | [] -> Lwt.return_ok acc
      | id :: tl ->
        kv#get
        >>= fun config ->
        match List.find_opt (fun (id', _) -> id = id') config.mode with
        | None ->
          Logs.debug (fun m ->
              m "Pull PLPs: configuration not found for tuner %d" id);
          aux acc tl
        | Some (id, mode) ->
          match mode.standard with
          (* We need to pull PLPs only if current standard is DVB-T2. *)
          | T | C -> aux acc tl
          | T2 ->
            request src rsp_queue sender (Request.Get_plp_list id)
            >>= function
            | Error _ as e -> Lwt.return e
            | Ok x -> aux (x :: acc) tl
    in
    aux [] tuners
  in
  detect
