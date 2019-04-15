open Board_dektec_dtm3200_types
open Netlib
open Request

let ( >>= ) = Lwt.bind

let reboot_steps = 10

let timeout = 3. (* seconds *)

let reboot_timeout = 30. (* seconds *)

type error =
  | Timeout
  | Not_responding
  | Fail of string

let error_to_string = function
  | Fail s -> s
  | Timeout -> "timeout"
  | Not_responding ->
    Printf.sprintf "no response from device after %d attempts" reboot_steps

let sleep timeout =
  Lwt_unix.sleep timeout
  >>= fun () -> Lwt.return_error Timeout

let loop (type a) stream (req : a Request.t) : (a, error) result Lwt.t =
  let rec aux () =
    Lwt_stream.next stream
    >>= fun x ->
    match Parser.is_response req x with
    | None -> aux ()
    | Some (Ok x) -> Lwt.return_ok x
    | Some (Error e) -> Lwt.return_error (Fail e) in
  Lwt_stream.junk_old stream >>= aux

let step ~(address : int)
    ~(return : unit -> unit Lwt.t)
    ~continue
    (src : Logs.src)
    (sender : Cstruct.t -> unit Lwt.t)
    (stream : Cstruct.t cmd Lwt_stream.t)
    ({ nw; ip } : config) =

  let (module Logs : Logs.LOG) = Logs.src_log src in

  let on_ok (type a) (req : a Request.t) to_string v =
    Logs.debug (fun m ->
        m "request '%s' accomplished succesfully with response = %s"
          (Request.to_string req) (to_string v)) in

  let on_error (type a) (req : a Request.t) (error : error) =
    Logs.warn (fun m ->
        m "request '%s' failed with error = %s"
          (to_string req) (error_to_string error)) in

  let rec detect_device req steps =
    sender @@ Serializer.make_req ~address req
    >>= fun () -> Lwt.pick [loop stream req; sleep timeout]
    >>= function
    | Error e ->
      on_error req e;
      if steps = 0
      then Lwt.return_error Not_responding
      else (
        Logs.debug (fun m -> m "no response, retrying...");
        detect_device req (pred steps))
    | Ok x -> Lwt.return_ok x in

  let get_network () =
    let request (type a) (req : a Request.t) to_string =
      sender @@ Serializer.make_req ~address req
      >>= fun () -> Lwt.pick [loop stream req; sleep timeout]
      >>= function
      | Ok x -> on_ok req to_string x; Lwt.return_ok x
      | Error e -> on_error req e; Lwt.return_error e in
    let ( >>= ) = Lwt_result.( >>= ) in
    request (Network (IP_address `R)) Ipaddr.V4.to_string
    >>= fun ip ->
    request (Network (Subnet_mask `R)) Ipaddr.V4.to_string
    >>= fun mask ->
    request (Network (Gateway `R)) Ipaddr.V4.to_string
    >>= fun gateway ->
    request (Network (DHCP `R)) string_of_bool
    >>= fun dhcp ->
    Lwt.return_ok { ip; mask; gateway; dhcp } in

  let rec set_mode () =
    let mode = IP2ASI in
    let req = Configuration (Mode (`W mode)) in
    sender @@ Serializer.make_req ~address req
    >>= fun () -> Lwt.pick [loop stream req; sleep timeout]
    >>= function
    | Error e -> on_error req e; return ()
    | Ok v ->
      on_ok req mode_to_string v;
      detect_device (Configuration (Mode `R)) reboot_steps
      >>= function
      | Error e ->
        Logs.warn (fun m -> m "%s, restarting..." @@ error_to_string e);
        return ()
      | Ok (mode' : mode) ->
        if equal_mode mode mode' then set_application ()
        else (
          Logs.warn (fun m ->
              m "failed setting application: expected %s, got %s"
                (mode_to_string mode) (mode_to_string mode'));
          return ())

  and set_application () =
    let app = Normal in
    let req = Configuration (Application (`W app)) in
    sender @@ Serializer.make_req ~address req
    >>= fun () -> Lwt.pick [loop stream req; sleep timeout]
    >>= function
    | Error e -> on_error req e; return ()
    | Ok v ->
      on_ok req application_to_string v;
      detect_device (Configuration (Application `R)) reboot_steps
      >>= function
      | Error e ->
        Logs.warn (fun m -> m "%s, restarting..." @@ error_to_string e);
        return ()
      | Ok (app' : application) ->
        if equal_application app app' then set_storage () else (
          Logs.err (fun m ->
              m "failed setting application: expected %s, got %s"
                (application_to_string app) (application_to_string app'));
          return ())

  and set_storage () =
    let v = FLASH in
    let req = Configuration (Volatile_storage (`W v)) in
    sender @@ Serializer.make_req ~address req
    >>= fun () -> Lwt.pick [loop stream req; sleep timeout]
    >>= function
    | Error e -> on_error req e; return ()
    | Ok (x : storage) ->
      on_ok req storage_to_string x;
      if equal_storage x v
      then check_network ()
      else (
        Logs.err (fun m ->
            m "failed setting volatile storage: expected %s, got %s"
              (storage_to_string x) (storage_to_string v));
        return ())

  and check_network () =
    get_network ()
    >>= function
    | Error e ->
      Logs.err (fun m -> m "error fetching network settings: %s"
                   (error_to_string e));
      return ()
    | Ok nw' ->
      (* If device network settings are equal to the user settings,
         skip network initialization and go directly to IP receiver
         initialization. *)
      if equal_nw nw' nw
      then (
        Logs.info (fun m -> m "no need to initialize network settings, skipping...");
        set_enable ())
      (* Otherwise, initialize network settings first. *)
      else (
        Logs.info (fun m -> m "initializing network settings...");
        set_ip_address nw')

  and set_ip_address nw' =
    if Ipaddr.V4.equal nw'.ip nw.ip
    then set_subnet_mask nw'
    else
      let req = Network (IP_address (`W nw.ip)) in
      sender @@ Serializer.make_req ~address req
      >>= fun () -> Lwt.pick [loop stream req; sleep timeout]
      >>= function
      | Error e -> on_error req e; return ()
      | Ok v -> on_ok req Ipaddr.V4.to_string v; set_subnet_mask nw'

  and set_subnet_mask nw' =
    if Ipaddr.V4.equal nw'.mask nw.mask
    then set_gateway nw'
    else
      let req = Network (Subnet_mask (`W nw.mask)) in
      sender @@ Serializer.make_req ~address req
      >>= fun () -> Lwt.pick [loop stream req; sleep timeout]
      >>= function
      | Error e -> on_error req e; return ()
      | Ok v -> on_ok req Ipaddr.V4.to_string v; set_gateway nw'

  and set_gateway nw' =
    if Ipaddr.V4.equal nw'.gateway nw.gateway
    then set_dhcp nw'
    else
      let req = Network (Gateway (`W nw.gateway)) in
      sender @@ Serializer.make_req ~address req
      >>= fun () -> Lwt.pick [loop stream req; sleep timeout]
      >>= function
      | Error e -> on_error req e; return ()
      | Ok v -> on_ok req Ipaddr.V4.to_string v; set_dhcp nw'

  and set_dhcp nw' =
    if nw'.dhcp = nw.dhcp
    then finalize_network ()
    else
      let req = Network (DHCP (`W nw.dhcp)) in
      sender @@ Serializer.make_req ~address req
      >>= fun () -> Lwt.pick [loop stream req; sleep timeout]
      >>= function
      | Error e -> on_error req e; return ()
      | Ok v -> on_ok req string_of_bool v; finalize_network ()

  (* Reboot device and check if the settings were applied. *)
  and finalize_network () =
    let req = Network Reboot in
    sender @@ Serializer.make_req ~address req
    >>= fun () -> Lwt.pick [loop stream req; sleep timeout]
    >>= function
    | Error e -> on_error req e; return ()
    | Ok () ->
      detect_device (Device FPGA_version) reboot_steps
      >>= function
      | Error e -> on_error req e; return ()
      | Ok _ ->
        get_network ()
        >>= function
        | Error _ -> return ()
        | Ok nw' ->
          let on_ok () =
            Logs.info (fun m -> m "network initialization done, \
                                   initializing IP receiver...");
            set_enable () in
          let on_error () =
            Logs.warn (fun m -> m "network initialization failed, \
                                   restarting...");
            return () in
          (* If DHCP is enabled, check only DHCP equivalence. *)
          if nw.dhcp
          then (
            if nw.dhcp = nw'.dhcp
            then on_ok ()
            else on_error ())
          (* Otherwise, check if all network settings are equal to expected. *)
          else if equal_nw nw' nw
          then on_ok ()
          else on_error ()

  and set_enable () =
    let req = IP_receive (Enable (`W ip.enable)) in
    sender @@ Serializer.make_req ~address req
    >>= fun () -> Lwt.pick [loop stream req; sleep timeout]
    >>= function
    | Error e -> on_error req e; return ()
    | Ok v -> on_ok req string_of_bool v; set_fec_enable ()

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
    | Error e -> on_error req e; return ()
    | Ok v ->
      on_ok req string_of_int v;
      match ip.multicast with
      | None -> set_addressing_method ()
      | Some x -> set_multicast x

  and set_multicast multicast =
    let req = IP_receive (Multicast_address (`W multicast)) in
    sender @@ Serializer.make_req ~address req
    >>= fun () -> Lwt.pick [loop stream req; sleep timeout]
    >>= function
    | Error e -> on_error req e; return ()
    | Ok v -> on_ok req Ipaddr.V4.to_string v; set_addressing_method ()

  and set_addressing_method () =
    let meth = match ip.multicast with
      | None -> Unicast
      | Some _ -> Multicast in
    let req = IP_receive (Addressing_method (`W meth)) in
    sender @@ Serializer.make_req ~address req
    >>= fun () -> Lwt.pick [loop stream req; sleep timeout]
    >>= function
    | Error e -> on_error req e; return ()
    | Ok x -> on_ok req meth_to_string x; set_delay ()

  and set_delay () =
    let req = IP_receive (IP_to_output_delay (`W ip.delay)) in
    sender @@ Serializer.make_req ~address req
    >>= fun () -> Lwt.pick [loop stream req; sleep timeout]
    >>= function
    | Error e -> on_error req e; return ()
    | Ok x -> on_ok req string_of_int x; set_rate_mode ()

  and set_rate_mode () =
    let req = IP_receive (Rate_estimation_mode (`W ip.rate_mode)) in
    sender @@ Serializer.make_req ~address req
    >>= fun () -> Lwt.pick [loop stream req; sleep timeout]
    >>= function
    | Error e -> on_error req e; return ()
    | Ok x -> on_ok req rate_mode_to_string x; continue ()

  in set_mode
