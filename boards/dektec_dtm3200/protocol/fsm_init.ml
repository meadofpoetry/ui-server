open Board_dektec_dtm3200_types
open Fsm_common

(* TODO check responses somehow. *)

let ( >>= ) = Lwt.bind

let step
    ~(return : unit -> unit Lwt.t)
    ~continue
    (src : Logs.src)
    (sender : Cstruct.t -> unit Lwt.t)
    (rsp_queue : Cstruct.t Request.cmd Lwt_stream.t)
    (config : config Kv_v.rw) =

  let (module Logs : Logs.LOG) = Logs.src_log src in

  let rec detect_device req steps =
    request src sender rsp_queue config req
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
    let req x = request src sender rsp_queue config x in
    req (Network (IP_address `R))
    >>= fun ip -> req (Network (Subnet_mask `R))
    >>= fun mask -> req (Network (Gateway `R))
    >>= fun gateway -> req (Network (DHCP `R))
    >>= fun dhcp -> Lwt.return_ok { ip; mask; gateway; dhcp } in

  let rec set_mode () =
    let mode = IP2ASI in
    let req = Request.Configuration (Mode (`W mode)) in
    Lwt_result.Infix.(
      request src sender rsp_queue config req
      (* FIXME what is here? maybe we don't need detect if the value is equal. *)
      >>= fun _ -> detect_device (Configuration (Mode `R)) reboot_steps
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
    | Error _ -> return ()

  and set_application () =
    let app = Normal in
    let req = Request.Configuration (Application (`W app)) in
    Lwt_result.Infix.(
      request src sender rsp_queue config req
      (* FIXME as above *)
      >>= fun _ -> detect_device (Configuration (Application `R)) reboot_steps
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
    | Error _ -> return ()

  and set_storage () =
    let v = FLASH in
    let req = Request.Configuration (Volatile_storage (`W v)) in
    Lwt_result.Infix.(
      request src sender rsp_queue config req
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
    config#get
    >>= fun { nw; ip; _ } ->
    get_network ()
    >>= function
    | Error _ -> return ()
    | Ok dev ->
      (* If device network settings are equal to the user settings,
         skip network initialization and go directly to IP receiver
         initialization. *)
      if equal_nw dev nw
      then (
        Logs.info (fun m -> m "No need to initialize network settings, skipping...");
        set_enable ip)
      (* Otherwise, initialize network settings first. *)
      else (
        Logs.info (fun m -> m "Initializing network settings...");
        set_ip_address ~cfg:nw ~dev)

  and set_ip_address ~cfg ~dev =
    if Netlib.Ipaddr.V4.equal dev.ip cfg.ip
    then set_subnet_mask ~cfg ~dev
    else
      let req = Request.Network (IP_address (`W cfg.ip)) in
      request src sender rsp_queue config req
      >>= function
      | Error _ -> return ()
      | Ok _ -> set_subnet_mask ~cfg ~dev

  and set_subnet_mask ~cfg ~dev =
    if Netlib.Ipaddr.V4.equal dev.mask cfg.mask
    then set_gateway ~cfg ~dev
    else
      let req = Request.Network (Subnet_mask (`W cfg.mask)) in
      request src sender rsp_queue config req
      >>= function
      | Error _ -> return ()
      | Ok _ -> set_gateway ~cfg ~dev

  and set_gateway ~cfg ~dev =
    if Netlib.Ipaddr.V4.equal dev.gateway cfg.gateway
    then set_dhcp ~cfg ~dev
    else
      let req = Request.Network (Gateway (`W cfg.gateway)) in
      request src sender rsp_queue config req
      >>= function
      | Error _ -> return ()
      | Ok _ -> set_dhcp ~cfg ~dev

  and set_dhcp ~cfg ~dev =
    if dev.dhcp = cfg.dhcp
    then finalize_network cfg
    else
      let req = Request.Network (DHCP (`W cfg.dhcp)) in
      request src sender rsp_queue config req
      >>= function
      | Error _ -> return ()
      | Ok _ -> finalize_network cfg

  (* Reboot device and check if the settings were applied. *)
  and finalize_network nw =
    let req = Request.Network Reboot in
    Lwt_result.Infix.(
      request src sender rsp_queue config req
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
    | Error _ -> return ()
    | Ok () ->
      config#get >>= fun { ip; _ } -> set_enable ip

  and set_enable cfg =
    let req = Request.IP_receive (Enable (`W cfg.enable)) in
    request src sender rsp_queue config req
    >>= function
    | Error _ -> return ()
    | Ok _ -> set_fec_enable cfg

  and set_fec_enable cfg =
    let req = Request.IP_receive (FEC_enable (`W cfg.fec)) in
    request src sender rsp_queue config req
    >>= function
    | Error _ -> return ()
    | Ok _ -> set_udp_port cfg

  and set_udp_port cfg =
    let req = Request.IP_receive (UDP_port (`W cfg.port)) in
    request src sender rsp_queue config req
    >>= function
    | Error _ -> return ()
    | Ok _ ->
      match cfg.multicast with
      | None -> set_addressing_method cfg
      | Some x -> set_multicast cfg x

  and set_multicast cfg multicast =
    let req = Request.IP_receive (Multicast_address (`W multicast)) in
    request src sender rsp_queue config req
    >>= function
    | Error _ -> return ()
    | Ok _ -> set_addressing_method cfg

  and set_addressing_method cfg =
    let meth = match cfg.multicast with
      | None -> Unicast
      | Some _ -> Multicast in
    let req = Request.IP_receive (Addressing_method (`W meth)) in
    request src sender rsp_queue config req
    >>= function
    | Error _ -> return ()
    | Ok _ -> set_delay cfg

  and set_delay cfg =
    let req = Request.IP_receive (IP_to_output_delay (`W cfg.delay)) in
    request src sender rsp_queue config req
    >>= function
    | Error _ -> return ()
    | Ok _ -> set_rate_mode cfg

  and set_rate_mode cfg =
    let req = Request.IP_receive (Rate_estimation_mode (`W cfg.rate_mode)) in
    request src sender rsp_queue config req
    >>= function
    | Error _ -> return ()
    | Ok _ -> continue ()
  in
  set_mode
