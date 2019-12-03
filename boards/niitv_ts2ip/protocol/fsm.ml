open Board_niitv_ts2ip_types
open Netlib

type api_msg = Request.msg Lwt_stream.t -> unit Lwt.t

type 'a set = ?step:React.step -> 'a -> unit

let ( >>= ) = Lwt.( >>= )

let cooldown_timeout = 10.

let status_timeout = 3.

let log_ok (type a) src (req : a Request.t) v =
  Logs.debug ~src (fun m ->
      let base = Printf.sprintf "Request \"%s\" succeeded" (Request.to_string req) in
      let s =
        match Request.value_to_string req v with
        | None -> base
        | Some v -> Printf.sprintf "%s. Response = %s" base v
      in
      m "%s" s)

let log_error (type a) src (req : a Request.t) (error : Request.error) =
  Logs.err ~src (fun m ->
      m
        "Request \"%s\" failed. Error = %s"
        (Request.to_string req)
        (Request.error_to_string error))

let loop (type a) (stream : Request.msg Lwt_stream.t) (req : a Request.t) :
    (a, Request.error) result Lwt.t =
  let rec aux () =
    Lwt_stream.next stream
    >>= fun x ->
    match Parser.is_response req x with
    | None -> aux ()
    | Some (Ok x) -> Lwt.return_ok x
    | Some (Error e) -> Lwt.return_error e
  in
  Lwt_stream.junk_old stream >>= aux

let sleep timeout = Lwt_unix.sleep timeout >>= fun () -> Lwt.return_error Request.Timeout

let request
    (type a)
    (src : Logs.src)
    (stream : Request.msg Lwt_stream.t)
    (sender : Cstruct.t -> unit Lwt.t)
    (req : a Request.t) : (a, Request.error) result Lwt.t =
  Logs.debug ~src (fun m -> m "Requesting \"%s\"" @@ Request.to_string req);
  sender @@ Serializer.serialize req
  >>= fun () ->
  (match req with
  | Get_devinfo -> Lwt.pick [loop stream req; sleep (Request.timeout req)]
  | Set_mode_main _ -> Lwt.return_ok ()
  | Set_mode_aux_1 _ -> Lwt.return_ok ()
  | Set_mode_aux_2 _ -> Lwt.return_ok ()
  | Set_mac _ -> Lwt.return_ok ())
  >>= function
  | Error e ->
      log_error src req e;
      Lwt.return_error e
  | Ok x ->
      log_ok src req x;
      Lwt.return_ok x

let start
    (src : Logs.src)
    (sender : Cstruct.t -> unit Lwt.t)
    (req_queue : api_msg Lwt_stream.t)
    (rsp_queue : Request.msg Lwt_stream.t)
    (evt_queue : Cstruct.t Lwt_stream.t)
    (mode_changer : mode Lwt_stream.t)
    (kv : config Kv_v.rw)
    (set_state : Application_types.Topology.state set)
    (set_devinfo : devinfo set)
    (set_device_status : device_status set)
    (set_transmitter_status : transmitter_status set) =
  let (module Logs : Logs.LOG) = Logs.src_log src in
  let log_udp_transmitters start = function
    | [] -> ()
    | l ->
        let stop = start + List.length l in
        Logs.debug (fun m -> m "Setting UDP transmitters (%d - %d):" start stop);
        List.iteri
          (fun i (x : udp_mode) ->
            Logs.debug (fun m ->
                m
                  "Setting UDP transmitter %d: stream = %s, IP = %a, port = %d, self \
                   port = %d, enabled = %b, socket = %s"
                  (start + i)
                  (show_stream x.stream)
                  Ipaddr.V4.pp
                  x.dst_ip
                  x.dst_port
                  x.self_port
                  x.enabled
                  (socket_to_string x.socket)))
          l
  in
  let rec restart () =
    Logs.info (fun m -> m "Restarting...");
    set_state `No_response;
    Lwt_stream.junk_old req_queue
    >>= fun () ->
    Lwt_stream.junk_old rsp_queue
    >>= fun () -> Lwt_unix.sleep cooldown_timeout >>= detect
  and detect () =
    Logs.info (fun m -> m "Start of connection establishment...");
    set_state `Detect;
    let rec loop () =
      Lwt.pick
        [ (Lwt_stream.next evt_queue >>= fun _ -> Lwt.return `S)
        ; (Lwt_stream.next rsp_queue
          >>= function
          | {tag = `Devinfo_rsp; data} -> Lwt.return @@ `D data
          | _ -> Lwt.return `N) ]
      >>= function
      | `S -> request src rsp_queue sender Request.Get_devinfo
      | `D data -> Lwt.return @@ Parser.parse_devinfo data
      | `N -> loop ()
    in
    loop ()
    >>= function
    | Ok x -> initialize x
    | Error e ->
        Logs.err (fun m ->
            m "Got error during detect step: %s" @@ Request.error_to_string e);
        restart ()
  and initialize (info : devinfo) =
    let step = React.Step.create () in
    set_state ~step `Init;
    set_devinfo ~step info;
    React.Step.execute step;
    Logs.info (fun m -> m "Connection established, device initialization started...");
    kv#get
    >>= fun {mac; mode} ->
    Logs.debug (fun m -> m "Setting MAC address: %a" Macaddr.pp mac);
    sender @@ Serializer.serialize Request.(Set_mac mac)
    >>= fun () ->
    let mode, aux_1, aux_2 = Request.split_mode mode in
    Logs.debug (fun m ->
        m
          "Setting network: IP = %a, mask = %a, gateway = %a"
          Ipaddr.V4.pp
          mode.network.ip
          Ipaddr.V4.pp
          mode.network.mask
          Ipaddr.V4.pp
          mode.network.gateway);
    log_udp_transmitters 0 mode.udp;
    sender @@ Serializer.serialize Request.(Set_mode_main mode)
    >>= fun () ->
    log_udp_transmitters Message.n_udp_main aux_1;
    sender @@ Serializer.serialize Request.(Set_mode_aux_1 aux_1)
    >>= fun () ->
    log_udp_transmitters Message.(n_udp_main + n_udp_aux) aux_2;
    sender @@ Serializer.serialize Request.(Set_mode_aux_2 aux_2)
    >>= fun () ->
    Lwt_stream.junk_old rsp_queue >>= fun () -> Lwt_stream.junk_old evt_queue >>= idle
  and idle () =
    set_state `Fine;
    Logs.info (fun m -> m "Initialization done!");
    Lwt.pick [status_loop (); mode_loop (); client_loop ()] >>= restart
  and mode_loop () =
    Lwt_stream.last_new mode_changer
    >>= fun mode ->
    let mode, aux_1, aux_2 = Request.split_mode mode in
    sender @@ Serializer.serialize Request.(Set_mode_main mode)
    >>= fun () ->
    sender @@ Serializer.serialize Request.(Set_mode_aux_1 aux_1)
    >>= fun () ->
    sender @@ Serializer.serialize Request.(Set_mode_aux_2 aux_2) >>= mode_loop
  and client_loop () =
    let rec wait_devinfo () =
      Lwt_stream.next rsp_queue
      >>= function
      | {tag = `Devinfo_rsp; _} -> Lwt.return `E
      | _ -> wait_devinfo ()
    in
    Lwt.pick
      [wait_devinfo (); (Lwt_stream.next req_queue >>= fun x -> Lwt.return (`REQ x))]
    >>= function
    | `REQ send -> send rsp_queue >>= client_loop
    | `E ->
        Logs.err (fun m ->
            m "Got devinfo during operation, seems the device is not responding...");
        Lwt.return ()
  and status_loop () =
    Lwt.pick
      [ (Lwt_stream.next evt_queue
        >>= fun data ->
        match Parser.parse_status data with
        | Ok x -> Lwt.return @@ `S x
        | Error e -> Lwt.return @@ `E e)
      ; (Lwt_unix.sleep status_timeout >>= fun () -> Lwt.return `Tm) ]
    >>= function
    | `S (d, t) ->
        let step = React.Step.create () in
        set_device_status ~step d;
        set_transmitter_status ~step t;
        React.Step.execute step;
        status_loop ()
    | `E e ->
        Logs.err (fun m -> m "Status parsing failed: %s" @@ Request.error_to_string e);
        Lwt.return ()
    | `Tm ->
        Logs.err (fun m ->
            m
              "Seems that the device is not responding, got no status for %g seconds"
              status_timeout);
        Lwt.return ()
  in
  detect
