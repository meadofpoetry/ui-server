open Board_dektec_dtm3200_types
open Netlib
open Request
open Fsm_common

let ( >>= ) = Lwt.bind

let timeout = 3. (* seconds *)

let step ~(address : int)
    ~(return : unit -> unit Lwt.t)
    ~continue
    (src : Logs.src)
    (sender : Cstruct.t -> unit Lwt.t)
    (stream : Cstruct.t cmd Lwt_stream.t)
    ({ nw; ip } : config) =

  let (module Logs : Logs.LOG) = Logs.src_log src in

  let rec detect_device req steps =
    sender @@ Serializer.make_req ~address req
    >>= fun () -> Lwt.pick [loop stream req; sleep timeout]
    >>= function
    | Error e ->
      log_error src req e;
      if steps = 0
      then Lwt.return_error Not_responding
      else (
        Logs.debug (fun m -> m "No response, retrying...");
        detect_device req (pred steps))
    | Ok x -> Lwt.return_ok x in

  let get_network () =
    let request (type a) (req : a Request.t) =
      sender @@ Serializer.make_req ~address req
      >>= fun () -> Lwt.pick [loop stream req; sleep timeout]
      >>= function
      | Ok x -> log_ok src req x; Lwt.return_ok x
      | Error e -> log_error src req e; Lwt.return_error e in
    let ( >>= ) = Lwt_result.( >>= ) in
    request (Network (IP_address `R))
    >>= fun ip -> request (Network (Subnet_mask `R))
    >>= fun mask -> request (Network (Gateway `R))
    >>= fun gateway -> request (Network (DHCP `R))
    >>= fun dhcp -> Lwt.return_ok { ip; mask; gateway; dhcp } in

  let rec set_mode () =
    let mode = IP2ASI in
    let req = Configuration (Mode (`W mode)) in
    sender @@ Serializer.make_req ~address req
    >>= fun () -> Lwt.pick [loop stream req; sleep timeout]
    >>= function
    | Error e -> log_error src req e; return ()
    | Ok v ->
      log_ok src req v;
      detect_device (Configuration (Mode `R)) reboot_steps
      >>= function
      | Error e ->
        Logs.warn (fun m -> m "%s, restarting..." @@ error_to_string e);
        return ()
      | Ok (mode' : mode) ->
        if equal_mode mode mode' then set_application ()
        else (
          Logs.warn (fun m ->
              m "Failed setting application: expected %s, got %s"
                (mode_to_string mode) (mode_to_string mode'));
          return ())

  and set_application () =
    let app = Normal in
    let req = Configuration (Application (`W app)) in
    sender @@ Serializer.make_req ~address req
    >>= fun () -> Lwt.pick [loop stream req; sleep timeout]
    >>= function
    | Error e -> log_error src req e; return ()
    | Ok v ->
      log_ok src req v;
      detect_device (Configuration (Application `R)) reboot_steps
      >>= function
      | Error e ->
        Logs.warn (fun m -> m "%s, restarting..." @@ error_to_string e);
        return ()
      | Ok (app' : application) ->
        if equal_application app app' then set_storage () else (
          Logs.err (fun m ->
              m "Failed setting application: expected %s, got %s"
                (application_to_string app) (application_to_string app'));
          return ())

  and set_storage () =
    let v = FLASH in
    let req = Configuration (Volatile_storage (`W v)) in
    sender @@ Serializer.make_req ~address req
    >>= fun () -> Lwt.pick [loop stream req; sleep timeout]
    >>= function
    | Error e -> log_error src req e; return ()
    | Ok (x : storage) ->
      log_ok src req x;
      if equal_storage x v
      then check_network ()
      else (
        Logs.err (fun m ->
            m "Failed setting volatile storage: expected %s, got %s"
              (storage_to_string x) (storage_to_string v));
        return ())

  and check_network () =
    get_network ()
    >>= function
    | Error e ->
      Logs.err (fun m -> m "Error fetching network settings: %s"
                   (error_to_string e));
      return ()
    | Ok nw' ->
      (* If device network settings are equal to the user settings,
         skip network initialization and go directly to IP receiver
         initialization. *)
      if equal_nw nw' nw
      then (
        Logs.info (fun m -> m "No need to initialize network settings, skipping...");
        set_enable ())
      (* Otherwise, initialize network settings first. *)
      else (
        Logs.info (fun m -> m "Initializing network settings...");
        set_ip_address nw')

  and set_ip_address nw' =
    if Ipaddr.V4.equal nw'.ip nw.ip
    then set_subnet_mask nw'
    else
      let req = Network (IP_address (`W nw.ip)) in
      sender @@ Serializer.make_req ~address req
      >>= fun () -> Lwt.pick [loop stream req; sleep timeout]
      >>= function
      | Error e -> log_error src req e; return ()
      | Ok v -> log_ok src req v; set_subnet_mask nw'

  and set_subnet_mask nw' =
    if Ipaddr.V4.equal nw'.mask nw.mask
    then set_gateway nw'
    else
      let req = Network (Subnet_mask (`W nw.mask)) in
      sender @@ Serializer.make_req ~address req
      >>= fun () -> Lwt.pick [loop stream req; sleep timeout]
      >>= function
      | Error e -> log_error src req e; return ()
      | Ok v -> log_ok src req v; set_gateway nw'

  and set_gateway nw' =
    if Ipaddr.V4.equal nw'.gateway nw.gateway
    then set_dhcp nw'
    else
      let req = Network (Gateway (`W nw.gateway)) in
      sender @@ Serializer.make_req ~address req
      >>= fun () -> Lwt.pick [loop stream req; sleep timeout]
      >>= function
      | Error e -> log_error src req e; return ()
      | Ok v -> log_ok src req v; set_dhcp nw'

  and set_dhcp nw' =
    if nw'.dhcp = nw.dhcp
    then finalize_network ()
    else
      let req = Network (DHCP (`W nw.dhcp)) in
      sender @@ Serializer.make_req ~address req
      >>= fun () -> Lwt.pick [loop stream req; sleep timeout]
      >>= function
      | Error e -> log_error src req e; return ()
      | Ok v -> log_ok src req v; finalize_network ()

  (* Reboot device and check if the settings were applied. *)
  and finalize_network () =
    let req = Network Reboot in
    sender @@ Serializer.make_req ~address req
    >>= fun () -> Lwt.pick [loop stream req; sleep timeout]
    >>= function
    | Error e -> log_error src req e; return ()
    | Ok () ->
      detect_device (Device FPGA_version) reboot_steps
      >>= function
      | Error e -> log_error src req e; return ()
      | Ok _ ->
        get_network ()
        >>= function
        | Error _ -> return ()
        | Ok nw' ->
          let log_ok src () =
            Logs.info (fun m -> m "Network initialization done, \
                                   initializing IP receiver...");
            set_enable () in
          let log_error src () =
            Logs.warn (fun m -> m "Network initialization failed, \
                                   restarting...");
            return () in
          (* If DHCP is enabled, check only DHCP equivalence. *)
          if nw.dhcp
          then (
            if nw.dhcp = nw'.dhcp
            then log_ok src ()
            else log_error src ())
          (* Otherwise, check if all network settings are equal to expected. *)
          else if equal_nw nw' nw
          then log_ok src ()
          else log_error src ()

  and set_enable () =
    let req = IP_receive (Enable (`W ip.enable)) in
    sender @@ Serializer.make_req ~address req
    >>= fun () -> Lwt.pick [loop stream req; sleep timeout]
    >>= function
    | Error e -> log_error src req e; return ()
    | Ok v -> log_ok src req v; set_fec_enable ()

  and set_fec_enable () =
    let req = IP_receive (FEC_enable (`W ip.fec)) in
    sender @@ Serializer.make_req ~address req
    >>= fun () -> Lwt.pick [loop stream req; sleep timeout]
    >>= function
    | Error _ -> return ()
    | Ok _ -> set_udp_port ()

  and set_udp_port () =
    let req = IP_receive (UDP_port (`W ip.port)) in
    sender @@ Serializer.make_req ~address req
    >>= fun () -> Lwt.pick [loop stream req; sleep timeout]
    >>= function
    | Error e -> log_error src req e; return ()
    | Ok v ->
      log_ok src req v;
      match ip.multicast with
      | None -> set_addressing_method ()
      | Some x -> set_multicast x

  and set_multicast multicast =
    let req = IP_receive (Multicast_address (`W multicast)) in
    sender @@ Serializer.make_req ~address req
    >>= fun () -> Lwt.pick [loop stream req; sleep timeout]
    >>= function
    | Error e -> log_error src req e; return ()
    | Ok v -> log_ok src req v; set_addressing_method ()

  and set_addressing_method () =
    let meth = match ip.multicast with
      | None -> Unicast
      | Some _ -> Multicast in
    let req = IP_receive (Addressing_method (`W meth)) in
    sender @@ Serializer.make_req ~address req
    >>= fun () -> Lwt.pick [loop stream req; sleep timeout]
    >>= function
    | Error e -> log_error src req e; return ()
    | Ok x -> log_ok src req x; set_delay ()

  and set_delay () =
    let req = IP_receive (IP_to_output_delay (`W ip.delay)) in
    sender @@ Serializer.make_req ~address req
    >>= fun () -> Lwt.pick [loop stream req; sleep timeout]
    >>= function
    | Error e -> log_error src req e; return ()
    | Ok x -> log_ok src req x; set_rate_mode ()

  and set_rate_mode () =
    let req = IP_receive (Rate_estimation_mode (`W ip.rate_mode)) in
    sender @@ Serializer.make_req ~address req
    >>= fun () -> Lwt.pick [loop stream req; sleep timeout]
    >>= function
    | Error e -> log_error src req e; return ()
    | Ok x -> log_ok src req x; continue ()
  in
  set_mode
