open Board_niitv_ts2ip_types
open Application_types

type api_msg = (Request.msg Lwt_stream.t -> unit Lwt.t)

let ( >>= ) = Lwt.( >>= )

let cooldown_timeout = 10.

let status_timeout = 3.

let log_ok (type a) src (req : a Request.t) v =
  Logs.debug ~src (fun m ->
      let base =
        Printf.sprintf "Request \"%s\" succeeded"
          (Request.to_string req) in
      let s = match Request.value_to_string req v with
        | None -> base
        | Some v -> Printf.sprintf "%s. Response = %s" base v in
      m "%s" s)

let log_error (type a) src (req : a Request.t) (error : Request.error) =
  Logs.err ~src (fun m ->
      m "Request \"%s\" failed. Error = %s"
        (Request.to_string req) (Request.error_to_string error))

let loop (type a)
    (stream : Request.msg Lwt_stream.t)
    (req : a Request.t) : (a, Request.error) result Lwt.t =
  let rec aux () =
    Lwt_stream.next stream
    >>= fun x ->
    match Parser.is_response req x with
    | None -> aux ()
    | Some (Ok x) -> Lwt.return_ok x
    | Some (Error e) -> Lwt.return_error e in
  (* FIXME remove junk old? *)
  Lwt_stream.junk_old stream >>= aux

let sleep timeout =
  Lwt_unix.sleep timeout
  >>= fun () -> Lwt.return_error Request.Timeout

let request (type a)
    (src : Logs.src)
    (stream : Request.msg Lwt_stream.t)
    (sender : Cstruct.t -> unit Lwt.t)
    (req : a Request.t) : (a, Request.error) result Lwt.t =
  sender @@ Serializer.make_req req
  >>= fun () ->
  (match req with
   | Get_devinfo -> Lwt.pick [loop stream req; sleep (Request.timeout req)]
   | Set_mode_main _ -> Lwt.return_ok ()
   | Set_mode_aux_1 _ -> Lwt.return_ok ()
   | Set_mode_aux_2 _ -> Lwt.return_ok ()
   | Set_mac _ -> Lwt.return_ok ())
  >>= function
  | Error e -> log_error src req e; Lwt.return_error e
  | Ok x -> log_ok src req x; Lwt.return_ok x

let start (src : Logs.src)
    (sender : Cstruct.t -> unit Lwt.t)
    (req_queue : api_msg Lwt_stream.t)
    (rsp_queue : Request.msg Lwt_stream.t)
    (kv : config Kv_v.rw)
    (set_state : Application_types.Topology.state -> unit)
    (set_devinfo : devinfo -> unit)
    (set_device_status : device_status -> unit)
    (set_transmitter_status : transmitter_status -> unit) =

  let (module Logs : Logs.LOG) = Logs.src_log src in

  let log_udp_transmitters start = function
    | [] -> ()
    | l ->
      let stop = start + List.length l in
      Logs.debug (fun m -> m "Setting UDP transmitters (%d - %d):" start stop);
      List.iteri (fun i (x : udp_mode) ->
          Logs.debug (fun m ->
              m "Setting UDP transmitter %d: stream = %s, IP = %a, \
                 port = %d, self port = %d, enabled = %b, socket = %s"
                (start + i)
                (Application_types.Stream.Multi_TS_ID.show x.stream)
                Netlib.Ipaddr.V4.pp x.dst_ip
                x.dst_port x.self_port x.enabled
                (socket_to_string x.socket))) l in

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
      | { tag = `Status; _ } -> request src rsp_queue sender Request.Get_devinfo
      | { tag = `Devinfo_rsp; data } -> Lwt.return @@ Parser.parse_devinfo data
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
    >>= fun { mac; mode } ->
    Logs.debug (fun m -> m "Setting MAC address: %a" Netlib.Macaddr.pp mac);
    Lwt_result.Infix.(
      request src rsp_queue sender Request.(Set_mac mac)
      >>= fun () ->
      let mode, aux_1, aux_2 = Request.split_mode mode in
      Logs.debug (fun m ->
          m "Setting network: IP = %a, mask = %a, gateway = %a"
            Netlib.Ipaddr.V4.pp mode.network.ip
            Netlib.Ipaddr.V4.pp mode.network.mask
            Netlib.Ipaddr.V4.pp mode.network.gateway);
      log_udp_transmitters 0 mode.udp;
      request src rsp_queue sender Request.(Set_mode_main mode)
      >>= fun () ->
      log_udp_transmitters Message.n_udp_main aux_1;
      request src rsp_queue sender Request.(Set_mode_aux_1 aux_1)
      >>= fun () ->
      log_udp_transmitters Message.(n_udp_main + n_udp_aux) aux_2;
      request src rsp_queue sender Request.(Set_mode_aux_2 aux_2))
    >>= function
    | Error e ->
      Logs.err (fun m ->
          m "Got error during init step: %s"
          @@ Request.error_to_string e);
      restart ()
    | Ok () ->
      Lwt_stream.junk_old rsp_queue
      >>= fun () ->
      set_state `Fine;
      Logs.info (fun m -> m "Initialization done!");
      idle ()

  and idle ?timer () =
    let rec loop () =
      Lwt_stream.next rsp_queue
      >>= function
      | { tag = `Devinfo_rsp; _ } -> Lwt.return `Devinfo
      | { tag = `Status; data } ->
        (match Parser.parse_status data with
         | Ok x -> Lwt.return (`S x)
         | Error e ->
           Logs.warn (fun m ->
               m "Status parsing failed: %s" @@ Request.error_to_string e);
           loop ())
      | _ -> loop () in
    let timer = match timer with
      | Some x -> x
      | None -> Lwt_unix.sleep status_timeout >>= fun () -> Lwt.return `Tm in
    let wait_status = loop () in
    let wait_client = Lwt_stream.next req_queue >>= fun x -> Lwt.return (`R x) in
    Lwt.choose [wait_status; wait_client; timer]
    >>= function
    | `R x -> Lwt.cancel wait_status; send_client_request timer x
    | `S (d, t) ->
      Lwt.cancel wait_client;
      set_device_status d;
      set_transmitter_status t;
      idle ()
    | `Devinfo ->
      Lwt.cancel wait_client;
      Logs.err (fun m ->
          m "Seems that the device has been reset, \
             got device info during operation");
      restart ()
    | `Tm ->
      Lwt.cancel wait_client;
      Lwt.cancel wait_status;
      Logs.err (fun m ->
          m "Seems that the device is not responding, \
             got no status for %g seconds" status_timeout);
      restart ()

  and send_client_request (type a) timer send =
    let stream = Lwt_stream.filter (function
        | { Request. tag = `Status; _ } -> false
        | _ -> true) rsp_queue in
    send stream >>= idle ~timer
  in
  first_step
