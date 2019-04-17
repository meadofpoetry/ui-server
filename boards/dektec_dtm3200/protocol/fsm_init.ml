open Board_dektec_dtm3200_types
open Fsm_common

(* TODO check responses somehow. *)

let ( >>= ) = Lwt.bind

let step ~(address : int)
    ~(return : unit -> unit Lwt.t)
    ~continue
    (src : Logs.src)
    (sender : Cstruct.t -> unit Lwt.t)
    (rsp_queue : Cstruct.t Request.cmd Lwt_stream.t)
    ({ nw; ip } : config) =

  let (module Logs : Logs.LOG) = Logs.src_log src in

  let rec detect_device req steps =
    request ~address src sender rsp_queue req
    >>= function
    | Ok x -> Lwt.return_ok x
    | Error _ ->
      if steps = 0
      then (
        Logs.err (fun m -> m "No response, restarting...");
        Lwt.return_error Request.Not_responding)
      else (
        Logs.debug (fun m -> m "No response, retrying...");
        detect_device req (pred steps)) in

  let get_network () =
    let ( >>= ) = Lwt_result.( >>= ) in
    let req x = request ~address src sender rsp_queue x in
    req (Network (IP_address `R))
    >>= fun ip -> req (Network (Subnet_mask `R))
    >>= fun mask -> req (Network (Gateway `R))
    >>= fun gateway -> req (Network (DHCP `R))
    >>= fun dhcp -> Lwt.return_ok { ip; mask; gateway; dhcp } in

  let rec set_mode () =
    let mode = IP2ASI in
    let req = Request.Configuration (Mode (`W mode)) in
    Lwt_result.Infix.(
      request ~address src sender rsp_queue req
      >>= fun v -> detect_device (Configuration (Mode `R)) reboot_steps
      >>= fun (mode' : mode) ->
      if equal_mode mode mode'
      then Lwt.return_ok ()
      else (
        let name = Request.to_string req in
        let got = mode_to_string mode' in
        let error = Request.Not_set (name, got) in
        Logs.err (fun m -> m "%s" @@ Request.error_to_string error);
        Lwt.return_error error))
    >>= function
    | Ok () -> set_application ()
    | Error e -> return ()

  and set_application () =
    let app = Normal in
    let req = Request.Configuration (Application (`W app)) in
    Lwt_result.Infix.(
      request ~address src sender rsp_queue req
      >>= fun v -> detect_device (Configuration (Application `R)) reboot_steps
      >>= fun (app' : application) ->
      if equal_application app app'
      then Lwt.return_ok ()
      else (
        let name = Request.to_string req in
        let got = application_to_string app' in
        let error = Request.Not_set (name, got) in
        Logs.err (fun m -> m "%s" @@ Request.error_to_string error);
        Lwt.return_error error))
    >>= function
    | Ok () -> set_storage ()
    | Error e -> return ()

  and set_storage () =
    let v = FLASH in
    let req = Request.Configuration (Volatile_storage (`W v)) in
    Lwt_result.Infix.(
      request ~address src sender rsp_queue req
      >>= fun (x : storage) ->
      if equal_storage x v
      then Lwt.return_ok ()
      else (
        let name = Request.to_string req in
        let got = storage_to_string x in
        let error = Request.Not_set (name, got) in
        Logs.err (fun m -> m "%s" @@ Request.error_to_string error);
        Lwt.return_error error))
    >>= function
    | Ok () -> check_network ()
    | Error _ -> return ()

  and check_network () =
    get_network ()
    >>= function
    | Error _ -> return ()
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
    if Netlib.Ipaddr.V4.equal nw'.ip nw.ip
    then set_subnet_mask nw'
    else
      let req = Request.Network (IP_address (`W nw.ip)) in
      request ~address src sender rsp_queue req
      >>= function
      | Error _ -> return ()
      | Ok v -> set_subnet_mask nw'

  and set_subnet_mask nw' =
    if Netlib.Ipaddr.V4.equal nw'.mask nw.mask
    then set_gateway nw'
    else
      let req = Request.Network (Subnet_mask (`W nw.mask)) in
      request ~address src sender rsp_queue req
      >>= function
      | Error _ -> return ()
      | Ok v -> set_gateway nw'

  and set_gateway nw' =
    if Netlib.Ipaddr.V4.equal nw'.gateway nw.gateway
    then set_dhcp nw'
    else
      let req = Request.Network (Gateway (`W nw.gateway)) in
      request ~address src sender rsp_queue req
      >>= function
      | Error _ -> return ()
      | Ok v -> set_dhcp nw'

  and set_dhcp nw' =
    if nw'.dhcp = nw.dhcp
    then finalize_network ()
    else
      let req = Request.Network (DHCP (`W nw.dhcp)) in
      request ~address src sender rsp_queue req
      >>= function
      | Error _ -> return ()
      | Ok v -> finalize_network ()

  (* Reboot device and check if the settings were applied. *)
  and finalize_network () =
    let req = Request.Network Reboot in
    Lwt_result.Infix.(
      request ~address src sender rsp_queue req
      >>= fun () -> detect_device (Device FPGA_version) reboot_steps
      >>= fun _ -> get_network ()
      >>= fun nw' ->
      (* If DHCP is enabled, check only DHCP equivalence. *)
      if (nw.dhcp && nw.dhcp = nw'.dhcp)
      || ((not nw.dhcp) && equal_nw nw nw')
      then (
        Logs.info (fun m ->
            m "Network initialization done, \
               initializing IP receiver...");
        Lwt.return_ok ())
      else (
        let err = Request.Not_set ("Set network settings", nw_to_string nw') in
        Logs.err (fun m -> m "%s" @@ Request.error_to_string err);
        Lwt.return_error err))
    >>= function
    | Ok () -> set_enable ()
    | Error _ -> return ()

  and set_enable () =
    let req = Request.IP_receive (Enable (`W ip.enable)) in
    request ~address src sender rsp_queue req
    >>= function
    | Error _ -> return ()
    | Ok _ -> set_fec_enable ()

  and set_fec_enable () =
    let req = Request.IP_receive (FEC_enable (`W ip.fec)) in
    request ~address src sender rsp_queue req
    >>= function
    | Error _ -> return ()
    | Ok _ -> set_udp_port ()

  and set_udp_port () =
    let req = Request.IP_receive (UDP_port (`W ip.port)) in
    request ~address src sender rsp_queue req
    >>= function
    | Error _ -> return ()
    | Ok _ ->
      match ip.multicast with
      | None -> set_addressing_method ()
      | Some x -> set_multicast x

  and set_multicast multicast =
    let req = Request.IP_receive (Multicast_address (`W multicast)) in
    request ~address src sender rsp_queue req
    >>= function
    | Error _ -> return ()
    | Ok _ -> set_addressing_method ()

  and set_addressing_method () =
    let meth = match ip.multicast with
      | None -> Unicast
      | Some _ -> Multicast in
    let req = Request.IP_receive (Addressing_method (`W meth)) in
    request ~address src sender rsp_queue req
    >>= function
    | Error _ -> return ()
    | Ok _ -> set_delay ()

  and set_delay () =
    let req = Request.IP_receive (IP_to_output_delay (`W ip.delay)) in
    request ~address src sender rsp_queue req
    >>= function
    | Error _ -> return ()
    | Ok _ -> set_rate_mode ()

  and set_rate_mode () =
    let req = Request.IP_receive (Rate_estimation_mode (`W ip.rate_mode)) in
    request ~address src sender rsp_queue req
    >>= function
    | Error _ -> return ()
    | Ok _ -> continue ()
  in
  set_mode
