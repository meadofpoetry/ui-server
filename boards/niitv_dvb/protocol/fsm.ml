open Board_niitv_dvb_types

let ( >>= ) = Lwt.( >>= )

let cooldown_timeout = 10. (* seconds *)

let meas_interval = 1 (* seconds *)

let params_interval = 5 (* seconds *)

type api_msg = (Cstruct.t Request.msg Lwt_stream.t -> unit Lwt.t)
               * (Request.error -> unit)

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

let loop (type a) stream (req : a Request.t) : (a, Request.error) result Lwt.t =
  let rec aux () =
    Lwt_stream.next stream
    >>= fun x ->
    match Parser.is_response req x with
    | None -> aux ()
    | Some (Ok x) -> Lwt.return_ok x
    | Some (Error e) -> Lwt.return_error e in
  Lwt_stream.junk_old stream >>= aux

let start (src : Logs.src)
    (sender : Cstruct.t -> unit Lwt.t)
    (req_queue : api_msg Lwt_stream.t)
    (rsp_queue : Cstruct.t Request.msg Lwt_stream.t)
    (kv : Device.config Kv_v.rw)
    (source_id : int)
    (set_state : Application_types.Topology.state -> unit)
    (set_devinfo : Device.info -> unit)
    (set_measures : (int * Measure.t ts) list -> unit)
    (set_params : (int * Params.t ts) list -> unit)
    (set_plps : (int * Plp_list.t ts) list -> unit) =

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
      then (cnt_params := 0; cnt_meas := 0; Lwt.return `Both)
      else if !cnt_meas mod meas_interval = 0
      then (cnt_meas := 0; Lwt.return `Meas)
      else if !cnt_params mod params_interval = 0
      then (cnt_params := 0; Lwt.return `Params)
      else f () in
    f in

  let rec first_step () =
    Logs.info (fun m -> m "Start of connection establishment...");
    let msgs' = Lwt_stream.get_available req_queue in
    List.iter (fun (_, stop) -> stop Request.Not_responding) msgs';
    Lwt_stream.junk_old req_queue
    >>= fun () ->
    set_state `No_response;
    detect_device ()

  and detect_device () =
    let req = Request.Get_devinfo in
    sender @@ Serializer.make_req req
    >>= fun () ->
    Lwt.pick [sleep (Request.timeout req); loop rsp_queue req]
    >>= function
    | Error e ->
      log_error src req e;
      Lwt_unix.sleep cooldown_timeout >>= first_step
    | Ok devinfo ->
      log_ok src req devinfo;
      set_state `Init;
      set_devinfo devinfo;
      Logs.info (fun m ->
          m "Connection established, device initialization started...");
      set_source_id devinfo.receivers

  and set_source_id tuners =
    kv#get
    >>= fun config ->
    let req = Request.Set_src_id source_id in
    sender @@ Serializer.make_req req
    >>= fun () ->
    Lwt.pick [sleep (Request.timeout req); loop rsp_queue req]
    >>= function
    | Error e -> log_error src req e; first_step ()
    | Ok id ->
      log_ok src req id;
      if id = source_id
      then init_device tuners config
      else (
        Logs.err (fun m ->
            m "Failure setting up source ID = %d. \
               Device returned ID = %d" source_id id);
        Lwt_unix.sleep cooldown_timeout >>= first_step)

  and init_device tuners config =
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
          sender @@ Serializer.make_req req
          >>= fun () ->
          Lwt.pick [sleep (Request.timeout req); loop rsp_queue req]
          >>= function
          | Error e -> log_error src req e; Lwt.return_error e
          | Ok x -> log_ok src req x; aux (x :: acc) tl) in
    aux [] config
    >>= function
    | Error _ -> Lwt_unix.sleep cooldown_timeout >>= first_step
    | Ok x ->
      kv#set (List.map Device.(fun (id, rsp) -> id, rsp.mode) x)
      >>= fun () ->
      Logs.info (fun m -> m "Initialization done!");
      set_state `Fine;
      fork tuners ()

  and fork ?timer tuners () =
    let timer = match timer with
      | Some x -> x
      | None -> wait_probes () in
    let wait_msg = Lwt_stream.next req_queue >>= fun x -> Lwt.return @@ `Message x in
    Lwt.choose [wait_msg; timer]
    >>= function
    | `Message msg -> send_client_request timer tuners msg
    | `Both ->
      Lwt.cancel wait_msg;
      (Lwt_result.Infix.(
          pull_measurements tuners
          >>= fun meas -> pull_parameters tuners
          >>= fun params -> pull_plps tuners
          >>= fun plps -> Lwt.return_ok (meas, params, plps))
       >>= function
       | Ok (meas, params, plps) ->
         set_measures meas;
         set_params params;
         set_plps plps;
         fork tuners ()
       | Error _ -> Lwt_unix.sleep cooldown_timeout >>= first_step)
    | `Meas ->
      Lwt.cancel wait_msg;
      (pull_measurements tuners
       >>= function
       | Ok meas ->
         set_measures meas;
         fork tuners ()
       | Error _ -> Lwt_unix.sleep cooldown_timeout >>= first_step)
    | `Params ->
      Lwt.cancel wait_msg;
      (Lwt_result.Infix.(
          pull_parameters tuners
          >>= fun params -> pull_plps tuners
          >>= fun plps -> Lwt.return_ok (params, plps))
       >>= function
       | Ok (params, plps) ->
         set_params params;
         set_plps plps;
         fork tuners ()
       | Error _ -> Lwt_unix.sleep cooldown_timeout >>= first_step)

  and send_client_request timer tuners (send, _) =
    send rsp_queue >>= fork ~timer tuners

  and pull_measurements tuners =
    let rec aux acc = function
      | [] -> Lwt.return_ok acc
      | id :: tl ->
        let req = Request.Get_measure id in
        sender @@ Serializer.make_req req
        >>= fun () ->
        Lwt.pick [sleep (Request.timeout req); loop rsp_queue req]
        >>= function
        | Error e -> log_error src req e; Lwt.return_error e
        | Ok x ->
          log_ok src req x;
          let ts = fst x, { data = snd x; timestamp = Ptime_clock.now () } in
          aux (ts :: acc) tl
    in
    aux [] tuners

  and pull_parameters tuners =
    let rec aux acc = function
      | [] -> Lwt.return_ok acc
      | id :: tl ->
        kv#get
        >>= fun config ->
        match List.find_opt (fun (id', _) -> id = id') config with
        | None ->
          Logs.debug (fun m ->
              m "Pull parameters: configuration not found for tuner %d" id);
          aux acc tl
        | Some (id, mode) ->
          match mode.standard with
          (* We need to pull parameters only if current standard is DVB-T2. *)
          | T | C -> aux acc tl
          | T2 ->
            let req = Request.Get_params id in
            sender @@ Serializer.make_req req
            >>= fun () ->
            Lwt.pick [sleep (Request.timeout req); loop rsp_queue req]
            >>= function
            | Error e -> log_error src req e; Lwt.return_error e
            | Ok x ->
              log_ok src req x;
              let ts = fst x, { data = snd x; timestamp = Ptime_clock.now () } in
              aux (ts :: acc) tl
    in
    aux [] tuners

  and pull_plps tuners =
    let rec aux acc = function
      | [] -> Lwt.return_ok acc
      | id :: tl ->
        kv#get
        >>= fun config ->
        match List.find_opt (fun (id', _) -> id = id') config with
        | None ->
          Logs.debug (fun m ->
              m "Pull PLPs: configuration not found for tuner %d" id);
          aux acc tl
        | Some (id, mode) ->
          match mode.standard with
          (* We need to pull PLPs only if current standard is DVB-T2. *)
          | T | C -> aux acc tl
          | T2 ->
            let req = Request.Get_plp_list id in
            sender @@ Serializer.make_req req
            >>= fun () ->
            Lwt.pick [sleep (Request.timeout req); loop rsp_queue req]
            >>= function
            | Error e -> log_error src req e; Lwt.return_error e
            | Ok x ->
              log_ok src req x;
              let ts = fst x, { data = snd x; timestamp = Ptime_clock.now () } in
              aux (ts :: acc) tl
    in
    aux [] tuners
  in
  first_step
