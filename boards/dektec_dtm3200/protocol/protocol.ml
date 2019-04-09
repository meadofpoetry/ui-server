open Application_types
open Boards
open Board_dektec_dtm3200_types
open Util_react

let ( >>= ) = Lwt.bind

type notifs =
  { streams : Stream.Raw.t list React.signal
  ; state : Topology.state React.signal
  ; status : status React.event
  ; config : config React.event
  }

type push_events =
  { state : Topology.state -> unit
  ; status : status -> unit
  ; devinfo : devinfo option -> unit
  ; config : config  -> unit
  }

type api =
  { address : int
  ; kv : config Kv_v.rw
  ; notifs : notifs
  ; channel : 'a. 'a Request.t -> ('a, error) Lwt_result.t
  ; loop : (Cstruct.t list -> 'c Board.cc Lwt.t as 'c) Board.cc
  }
and error =
  | Not_responding
  | Timeout

let timeout = 3. (* seconds *)

(* type event_raw = [ `Error of parsed | `Ok of parsed ]
 * type pool = (event_raw,event) Pool.t
 * 
 * let period = Boards.Timer.steps ~step_duration:M.duration 1. *)

(* type t =
 *   { pool : pool
 *   ; prev : status option
 *   ; timer : int
 *   ; events : event list
 *   } *)

(* let merge ({ events; prev; _ } : t) : status =
 *   let get = fun f e -> Option.get_exn @@ List.find_map f e in
 *   let fec_delay = get (function Fec_delay x -> Some x | _ -> None) events in
 *   let fec_cols = get (function Fec_cols x -> Some x | _ -> None) events in
 *   let fec_rows = get (function Fec_rows x -> Some x | _ -> None) events in
 *   let jitter_tol = get (function Jitter_tol x -> Some x | _ -> None) events in
 *   let lost_after_fec =
 *     let x = get (function Lost_after_fec x -> Some x | _ -> None) events in
 *     match prev with
 *     | Some p -> Int64.(x - p.lost_after_fec)
 *     | None -> x in
 *   let lost_before_fec =
 *     let x = get (function Lost_before_fec x -> Some x | _ -> None) events in
 *     match prev with
 *     | Some p -> Int64.(x - p.lost_before_fec)
 *     | None -> x in
 *   let tp_per_ip = get (function Tp_per_ip x -> Some x | _ -> None) events in
 *   let status = get (function Status x -> Some x | _ -> None) events in
 *   let protocol = get (function Protocol x -> Some x | _ -> None) events in
 *   let packet_size = get (function Packet_size x -> Some x | _ -> None) events in
 *   let bitrate = get (function Bitrate x -> Some x | _ -> None) events in
 *   let pcr_present = get (function Pcr_present x -> Some x | _ -> None) events in
 *   let rate_change_cnt =
 *     let x = get (function Rate_change_cnt x -> Some x | _ -> None) events in
 *     match prev with
 *     | Some p -> Int32.sub x p.rate_change_cnt
 *     | None -> x in
 *   let jitter_err_cnt =
 *     let x = get (function Jitter_err_cnt x -> Some x | _ -> None) events in
 *     match prev with
 *     | Some p -> Int32.sub x p.jitter_err_cnt
 *     | None -> x in
 *   let lock_err_cnt =
 *     let x = get (function Lock_err_cnt x -> Some x | _ -> None) events in
 *     match prev with
 *     | Some p -> Int32.sub x p.lock_err_cnt
 *     | None -> x in
 *   let delay_factor = get (function Delay_factor x -> Some x | _ -> None) events in
 *   let asi_bitrate = get (function Asi_bitrate x -> Some x | _ -> None) events in
 *   { fec_delay
 *   ; fec_cols
 *   ; fec_rows
 *   ; jitter_tol
 *   ; lost_after_fec
 *   ; lost_before_fec
 *   ; tp_per_ip
 *   ; status
 *   ; protocol
 *   ; packet_size
 *   ; bitrate
 *   ; pcr_present
 *   ; rate_change_cnt
 *   ; jitter_err_cnt
 *   ; lock_err_cnt
 *   ; delay_factor
 *   ; asi_bitrate
 *   }
 * 
 * let make_pool () : pool =
 *   List.map (fun x ->
 *       { send = (fun () -> M.send x)
 *       ; pred = (is_response x)
 *       ; timeout = M.timeout
 *       ; exn = None })
 *     [ Ip Get_fec_delay
 *     ; Ip Get_fec_cols
 *     ; Ip Get_fec_rows
 *     ; Ip Get_jitter_tol
 *     ; Ip Get_bitrate
 *     ; Ip Get_lost_after_fec
 *     ; Ip Get_lost_before_fec
 *     ; Ip Get_tp_per_ip
 *     ; Ip Get_status
 *     ; Ip Get_protocol
 *     ; Ip Get_packet_size
 *     ; Ip Get_pcr_present
 *     ; Ip Get_rate_change_cnt
 *     ; Ip Get_jitter_err_cnt
 *     ; Ip Get_lock_err_cnt
 *     ; Ip Get_delay_factor
 *     ; Asi Get_bitrate ]
 *   |> Pool.create
 * 
 * let responsed (t : t) rsp = Pool.responsed t.pool rsp
 * let ready (t : t) = not (Pool.empty t.pool || t.timer > 0)
 * let send (t : t) = Pool.send t.pool ()
 * let cons (e : event) (t : t) = { t with events = e :: t.events }
 * let last (t : t) = Pool.last t.pool
 * let next (t : t) = { t with pool = Pool.next t.pool }
 * let wait (t : t) =
 *   if t.timer <= 0
 *   then { t with timer = 0 }
 *   else { t with timer = pred t.timer }
 * let step (t : t) = { t with pool = Pool.step t.pool } |> wait
 * let handle (pe : push_events) (t : t) : t =
 *   let status = merge t in
 *   if Option.is_some t.prev then pe.status status;
 *   { timer = period
 *   ; prev = Some status
 *   ; events = List.empty
 *   ; pool = Pool.init t.pool
 *   }
 * let make () : t =
 *   { pool = make_pool ()
 *   ; timer = 0
 *   ; prev = None
 *   ; events = List.empty
 *   } *)

let send_msg (type a) ~address sender (msg : a Request.t) : unit Lwt.t =
  let buf = Serializer.make_req ~address msg in
  sender buf

(* let send (type a) state msgs sender
 *       (storage : config storage) (pe : push_events)
 *       timeout (msg : a request) : a Lwt.t =
 *   match React.S.value state with
 *   | `Fine ->
 *      let t, w = Lwt.wait () in
 *      let pred = function
 *        | `Timeout -> Lwt.wakeup_exn w (Failure "msg timeout"); None
 *        | l ->
 *           let open Option in
 *           is_response msg l >|= fun r ->
 *           (\* FIXME previous value in response, investigate why *\)
 *           let c = storage#get in
 *           let c = match msg with
 *             | Nw (Set_ip ip) ->
 *                Some {c with nw = {c.nw with ip}}
 *             | Nw (Set_mask mask) ->
 *                Some {c with nw = {c.nw with mask}}
 *             | Nw (Set_gateway gateway) ->
 *                Some {c with nw = {c.nw with gateway}}
 *             | Nw (Set_dhcp dhcp) ->
 *                Some {c with nw = {c.nw with dhcp}}
 *             | Ip (Set_enable enable) ->
 *                Some {c with ip = {c.ip with enable}}
 *             | Ip (Set_fec_enable fec) ->
 *                Some {c with ip = {c.ip with fec}}
 *             | Ip (Set_udp_port port) ->
 *                Some {c with ip = {c.ip with port}}
 *             | Ip (Set_method x) ->
 *                begin match x with
 *                | Unicast -> Some {c with ip = {c.ip with multicast = None }}
 *                | Multicast -> None
 *                end
 *             | Ip (Set_mcast_addr x) ->
 *                Some {c with ip = {c.ip with multicast = Some x}}
 *             | Ip (Set_delay delay) ->
 *                Some {c with ip = {c.ip with delay}}
 *             | Ip (Set_rate_est_mode rate_mode) ->
 *                Some {c with ip = {c.ip with rate_mode}}
 *             | _ -> None
 *           in
 *           Option.iter (fun c -> pe.config c; storage#store c) c;
 *           Lwt.wakeup w r
 *      in
 *      let send = fun () -> send_msg sender msg in
 *      msgs := Queue.append !msgs { send; pred; timeout; exn = None };
 *      t
 *   | _ -> Lwt.fail (Failure "board is not responding") *)

let make_req ~address sender req =
  Pools.make_msg
    ~timeout:(fun () -> Lwt_unix.timeout timeout)
    ~send:(fun () -> send_msg ~address sender req)
    ~resolve:(Parser.is_response req)
    ()

let step ~(address : int)
      (src : Logs.src)
      msgs
      (sender : Cstruct.t -> unit Lwt.t)
      (config : config React.signal)
      (pe : push_events) =

  let reboot_steps = 7 in

  let deserialize acc recvd =
    let recvd = Board.concat_acc acc recvd in
    Parser.deserialize ~address src recvd in

  let rec first_step () =
    Logs.info ~src (fun m -> m "start of connection establishment...");
    msgs := Pools.Queue.invalidate !msgs;
    pe.state `No_response;
    let msg = make_req ~address sender (Request.Device FPGA_version) in
    Pools.Pool.(send (create [msg]))
    >>= fun pool ->
    Lwt.return @@ `Continue (step_detect_fpga_ver pool None)

  and match_detect ~pending ~success to_string name =
    Pools.Pool._match
      ~resolved:(fun _ -> function
        | `Error e ->
           Logs.warn ~src (fun m ->
               m "detect step - error getting %s: %s" name e);
           first_step ()
        | `Value x ->
           Logs.debug ~src (fun m ->
               m "detect step - got %s (%s)" name (to_string x));
           success x)
      ~error:(fun _ -> function
        | `Timeout ->
           Logs.warn ~src (fun m ->
               m "detect step - timeout reached while getting %s" name);
           first_step ())
      ~pending
      ~not_sent:(fun _ -> assert false)

  and step_detect_fpga_ver pool acc recvd =
    let responses, acc = deserialize acc recvd in
    Pools.Pool.apply pool responses;
    match_detect
      ~pending:(fun pool ->
        Lwt.return @@ `Continue (step_detect_fpga_ver pool acc))
      ~success:(fun x ->
        let msg = make_req ~address sender (Request.Device Hardware_version) in
        Pools.Pool.(send (create [msg]))
        >>= fun pool ->
        Lwt.return @@ `Continue (step_detect_hw_ver x pool None))
      string_of_int "FPGA version"

  and step_detect_hw_ver conf pool acc recvd =
    let responses, acc = deserialize acc recvd in
    Pools.Pool.apply pool responses;
    match_detect
      ~pending:(fun pool ->
        Lwt.return @@ `Continue (step_detect_hw_ver conf pool acc))
      ~success:(fun x ->
        let msg = make_req ~address sender (Request.Device Firmware_version) in
        Pools.Pool.(send (create [msg]))
        >>= fun pool ->
        Lwt.return @@ `Continue (step_detect_fw_ver (conf, x) pool acc))
      string_of_int "hardware version"

  and step_detect_fw_ver ((fpga, hw) as conf) pool acc recvd =
    let responses, acc = deserialize acc recvd in
    Pools.Pool.apply pool responses;
    match_detect
      ~pending:(fun pool ->
        Lwt.return @@ `Continue (step_detect_fw_ver conf pool acc))
      ~success:(fun x ->
        let msg = make_req ~address sender (Request.Device Serial_number) in
        Pools.Pool.(send (create [msg]))
        >>= fun pool ->
        Lwt.return @@ `Continue (step_detect_serial (fpga, hw, x) None))
      string_of_int "firmware version"

  and step_detect_serial ((fpga, hw, fw) as conf) acc recvd =
    find_resp timer req acc recvd
      ~on_success:(fun x _ ->
        Logs.debug (fun m -> m "detect step - got serial: %d" x);
        let r = Devinfo Get_type in
        send r;
        `Continue (step_detect_type (Timer.reset timer) r
                     (fpga, hw, fw, x) None))
      ~on_failure:(fun timer acc ->
        `Continue (step_detect_serial timer req conf acc))
      ~on_timeout

  and step_detect_type (timer : Timer.t) req ((fpga, hw, fw, ser) as conf) acc recvd =
    find_resp timer req acc recvd
      ~on_success:(fun x _ ->
        Logs.debug (fun m -> m "detect step - got type: %d" x);
        let r = Nw Get_mac in
        send r;
        `Continue (step_detect_mac (Timer.reset timer) r
                     (fpga, hw, fw, ser, x) None))
      ~on_failure:(fun timer acc ->
        `Continue (step_detect_type timer req conf acc))
      ~on_timeout

  and step_detect_mac (timer : Timer.t) req conf acc recvd =
    find_resp timer req acc recvd
      ~on_success:(fun mac _ ->
        Logs.debug (fun m -> m "detect step - got mac: %s"
                             @@ Macaddr.to_string mac);
        let fpga_ver, hw_ver, fw_ver, serial, typ = conf in
        let r = Overall (Set_mode Ip2asi) in
        pe.devinfo (Some { fpga_ver; hw_ver; fw_ver; serial; typ; mac });
        pe.state `Init;
        Logs.info (fun m -> m "connection established, \
                               board initialization started...");
        send r;
        `Continue (step_init_mode (Timer.reset timer) r None))
      ~on_failure:(fun timer acc ->
        `Continue (step_detect_mac timer req conf acc))
      ~on_timeout

  and step_detect_after_init ns (timer : Timer.t) steps req acc recvd =
    find_resp timer req acc recvd
      ~on_success:(fun _ _ ->
        `Continue (step_wait_after_detect ns
                     (Timer.create ~step_duration timeout)))
      ~on_failure:(fun timer acc ->
        `Continue (step_detect_after_init ns timer steps req acc))
      ~on_timeout:(fun timer acc ->
        if steps > 0
        then (send_msg sender req |> Lwt.ignore_result;
              `Continue (step_detect_after_init ns
                           (Timer.reset timer) (pred steps) req acc))
        else on_timeout timer acc)

  and step_wait_after_detect ns (timer : Timer.t) _ =
    try
      `Continue (step_wait_after_detect ns (Timer.step timer))
    with
    | Timer.Timeout _ ->
       let timer = Timer.create ~step_duration timeout in
       begin match ns with
       | `Mode ->
          let r = Overall (Set_application Normal) in
          send r;
          `Continue (step_init_application timer r None)
       | `App ->
          let r = Overall (Set_storage Flash) in
          send r;
          `Continue (step_init_storage timer r None)
       | `Nw  ->
          let r = Ip (Set_enable storage#get.ip.enable) in
          send r;
          `Continue (step_init_ip_enable timer r None)
       end

  and step_init_mode (timer : Timer.t) req acc recvd =
    find_resp timer req acc recvd
      ~on_success:(fun x _ ->
        Logs.debug (fun m ->
            m "init step - mode set: %s" @@ show_mode x);
        let r = Devinfo Get_fpga_ver in
        send r;
        `Continue (step_detect_after_init `Mode
                     (Timer.reset timer) reboot_steps r None))
      ~on_failure:(fun timer acc ->
        `Continue (step_init_mode timer req acc))
      ~on_timeout

  and step_init_application (timer : Timer.t) req acc recvd =
    find_resp timer req acc recvd
      ~on_success:(fun x _ ->
        Logs.debug (fun m ->
            m "init step - application set: %s" @@ show_application x);
        let r = Devinfo Get_fpga_ver in
        send r;
        `Continue (step_detect_after_init `App
                     (Timer.reset timer) reboot_steps r None))
      ~on_failure:(fun timer acc ->
        `Continue (step_init_application timer req acc))
      ~on_timeout

  and step_init_storage (timer : Timer.t) req acc recvd =
    find_resp timer req acc recvd
      ~on_success:(fun x _ ->
        Logs.debug (fun m ->
            m "init step - storage set: %s" @@ show_storage x);
        let r = Nw Get_ip in
        send r;
        `Continue (step_get_ip (Timer.reset timer) r None))
      ~on_failure:(fun timer acc -> `Continue (step_init_storage timer req acc))
      ~on_timeout

  and step_get_ip (timer : Timer.t) req acc recvd =
    find_resp timer req acc recvd
      ~on_success:(fun resp _ ->
        Logs.debug (fun m -> m "init step - got ip: %s"
                             @@ Ipaddr.V4.to_string resp);
        if Ipaddr.V4.equal resp storage#get.nw.ip
        then (
          let r = Nw Get_mask in
          Logs.debug (fun m -> m "init step - no need to set ip, \
                                  skipping...");
          send r; `Continue (step_get_mask false (Timer.reset timer) r None))
        else (
          let r = Nw (Set_ip storage#get.nw.ip) in
          send r;
          `Continue (step_init_ip (Timer.reset timer) r None)))
      ~on_failure:(fun timer acc -> `Continue (step_get_ip timer req acc))
      ~on_timeout

  and step_init_ip (timer:Timer.t) req acc recvd =
    find_resp timer req acc recvd
      ~on_success:(fun x _ ->
        Logs.debug (fun m -> m "init step - ip set: %s"
                             @@ Ipaddr.V4.to_string x);
        let r = Nw Get_mask in
        send r;
        `Continue (step_get_mask true (Timer.reset timer) r None))
      ~on_failure:(fun timer acc -> `Continue (step_init_ip timer req acc))
      ~on_timeout

  and step_get_mask need_reboot (timer:Timer.t) req acc recvd =
    find_resp timer req acc recvd
      ~on_success:(fun resp _ ->
        Logs.debug (fun m -> m "init step - got mask: %s"
                             @@ Ipaddr.V4.to_string resp);
        if Ipaddr.V4.equal resp storage#get.nw.mask
        then (
          let r = Nw Get_gateway in
          Logs.debug (fun m -> m "init step - no need to set mask, \
                                  skipping...");
          send r;
          `Continue (step_get_gateway need_reboot (Timer.reset timer) r None))
        else (
          let r = Nw (Set_mask storage#get.nw.mask) in
          send r;
          `Continue (step_init_mask (Timer.reset timer) r None)))
      ~on_failure:(fun timer acc ->
        `Continue (step_get_mask need_reboot timer req acc))
      ~on_timeout

  and step_init_mask (timer:Timer.t) req acc recvd =
    find_resp timer req acc recvd
      ~on_success:(fun x _ ->
        Logs.debug (fun m -> m "init step - mask set: %s"
                             @@ Ipaddr.V4.to_string x);
        let r = Nw Get_gateway in
        send r;
        `Continue (step_get_gateway true (Timer.reset timer) r None))
      ~on_failure:(fun timer acc ->
        `Continue (step_init_mask timer req acc))
      ~on_timeout

  and step_get_gateway need_reboot (timer:Timer.t) req acc recvd =
    find_resp timer req acc recvd
      ~on_success:(fun resp _ ->
        Logs.debug (fun m -> m "init step - got gateway: %s"
                             @@ Ipaddr.V4.to_string resp);
        if Ipaddr.V4.equal resp storage#get.nw.gateway
        then (
          let r = Nw Get_dhcp in
          Logs.debug (fun m -> m "init step - no need to set gateway, \
                                  skipping...");
          send r;
          `Continue (step_get_dhcp need_reboot (Timer.reset timer) r None))
        else (
          let r = Nw (Set_gateway storage#get.nw.gateway) in
          send r;
          `Continue (step_init_gateway (Timer.reset timer) r None)))
      ~on_failure:(fun timer acc ->
        `Continue (step_get_gateway need_reboot timer req acc))
      ~on_timeout

  and step_init_gateway (timer:Timer.t) req acc recvd =
    find_resp timer req acc recvd
      ~on_success:(fun x _ ->
        Logs.debug (fun m -> m "init step - gateway set: %s"
                             @@ Ipaddr.V4.to_string x);
        let r = Nw Get_dhcp in
        send r; `Continue (step_get_dhcp true (Timer.reset timer) r None))
      ~on_failure:(fun timer acc ->
        `Continue (step_init_gateway timer req acc))
      ~on_timeout

  and step_get_dhcp need_reboot (timer:Timer.t) req acc recvd =
    find_resp timer req acc recvd
      ~on_success:(fun resp _ ->
        Logs.debug (fun m -> m "init step - got DHCP: %s"
                             @@ string_of_bool resp);
        let timer = Timer.reset timer in
        if Equal.bool resp storage#get.nw.dhcp
        then
          (Logs.debug (fun m -> m "init step - no need to set DHCP, \
                                   skipping...");
           step_need_reboot timer need_reboot)
        else (let r = Nw (Set_dhcp storage#get.nw.dhcp) in
              send r; `Continue (step_init_dhcp timer r None)))
      ~on_failure:(fun timer acc ->
        `Continue (step_get_dhcp need_reboot timer req acc))
      ~on_timeout


  and step_init_dhcp (timer:Timer.t) req acc recvd =
    find_resp timer req acc recvd
      ~on_success:(fun x _ ->
        Logs.debug (fun m -> m "init step - DHCP set: %s"
                             @@ string_of_bool x);
        step_need_reboot (Timer.reset timer) true)
      ~on_failure:(fun timer acc ->
        `Continue (step_init_dhcp timer req acc))
      ~on_timeout

  and step_need_reboot (timer:Timer.t) = function
    | true ->
       Logs.debug (fun m -> m "init step - rebooting...");
       let r = Nw Reboot in
       send r;
       `Continue (step_finalize_nw_init timer r None)
    | false ->
       let r = Ip (Set_enable storage#get.ip.enable) in
       send r;
       `Continue (step_init_ip_enable timer r None)

  and step_finalize_nw_init (timer:Timer.t) req acc recvd =
    find_resp timer req acc recvd
      ~on_success:(fun _ _ ->
        let r = Devinfo Get_fpga_ver in
        send r;
        `Continue (step_detect_after_init `Nw (Timer.reset timer)
                     reboot_steps r None))
      ~on_failure:(fun timer acc ->
        `Continue (step_finalize_nw_init timer req acc))
      ~on_timeout

  and step_init_ip_enable (timer:Timer.t) req acc recvd =
    find_resp timer req acc recvd
      ~on_success:(fun x _ ->
        Logs.debug (fun m -> m "init step - ip enable set: %s"
                             @@ string_of_bool x);
        let r = Ip (Set_fec_enable storage#get.ip.fec) in
        send r; `Continue (step_init_ip_fec (Timer.reset timer) r None))
      ~on_failure:(fun timer acc -> `Continue (step_init_ip_enable timer req acc))
      ~on_timeout

  and step_init_ip_fec (timer:Timer.t) req acc recvd =
    find_resp timer req acc recvd
      ~on_success:(fun x _ ->
        Logs.debug (fun m ->
            m "init step - fec enable set: %s" @@ string_of_bool x);
        let r = Ip (Set_udp_port storage#get.ip.port) in
        send r;
        `Continue (step_init_ip_udp_port (Timer.reset timer)
                     r None))
      ~on_failure:(fun timer acc ->
        `Continue (step_init_ip_fec timer req acc))
      ~on_timeout

  and step_init_ip_udp_port (timer:Timer.t) req acc recvd =
    find_resp timer req acc recvd
      ~on_success:(fun x _ ->
        Logs.debug (fun m ->
            m "init step - udp port set: %s" @@ string_of_int x);
        (match storage#get.ip.multicast with
         | Some x ->
            let r = Ip (Set_mcast_addr x) in
            send r;
            `Continue (step_init_ip_multicast (Timer.reset timer) r None)
         | None   ->
            let r = Ip (Set_method Unicast) in
            send r;
            `Continue (step_init_ip_method timer r None)))
      ~on_failure:(fun timer acc ->
        `Continue (step_init_ip_udp_port timer req acc))
      ~on_timeout

  and step_init_ip_multicast (timer:Timer.t) req acc recvd =
    find_resp timer req acc recvd
      ~on_success:(fun x _ ->
        Logs.debug (fun m ->
            m "init step - multicast address set: %s"
            @@ Ipaddr.V4.to_string x);
        let r = Ip (Set_method Multicast) in
        send r;
        `Continue (step_init_ip_method (Timer.reset timer) r None))
      ~on_failure:(fun timer acc ->
        `Continue (step_init_ip_multicast timer req acc))
      ~on_timeout

  and step_init_ip_method (timer:Timer.t) req acc recvd =
    find_resp timer req acc recvd
      ~on_success:(fun x _ ->
        Logs.debug (fun m ->
            m "init step - ip method set: %s" @@ show_meth x);
        let r = Ip (Set_delay storage#get.ip.delay) in
        send r;
        `Continue (step_init_ip_delay (Timer.reset timer) r None))
      ~on_failure:(fun timer acc ->
        `Continue (step_init_ip_method timer req acc))
      ~on_timeout

  and step_init_ip_delay (timer:Timer.t) req acc recvd =
    find_resp timer req acc recvd
      ~on_success:(fun x _ ->
        Logs.debug (fun m ->
            m "init step - ip delay set: %s" @@ string_of_int x);
        let r = Ip (Set_rate_est_mode storage#get.ip.rate_mode) in
        send r;
        `Continue (step_init_ip_rate_mode (Timer.reset timer) r None))
      ~on_failure:(fun timer acc ->
        `Continue (step_init_ip_delay timer req acc))
      ~on_timeout

  and step_init_ip_rate_mode (timer:Timer.t) req acc recvd =
    find_resp timer req acc recvd
      ~on_success:(fun x _ ->
        Logs.debug (fun m ->
            m "init step - ip rate mode set: %s" @@ show_rate_mode x);
        pe.state `Fine;
        Logs.info (fun m -> m "initialization done!");
        let probes = Probes.make () in
        `Continue (step_ok_probes_send probes None))
      ~on_failure:(fun timer acc ->
        `Continue (step_init_ip_rate_mode timer req acc))
      ~on_timeout

  and step_ok_tee probes acc recvd =
    match Queue.empty !msgs with
    | true  -> step_ok_probes_send probes acc recvd
    | false -> step_ok_requests_send probes acc recvd

  and step_ok_probes_send probes acc _ =
    let probes = Probes.wait probes in
    if Probes.ready probes
    then (Probes.send probes |> ignore;
          `Continue (step_ok_probes_wait probes acc))
    else `Continue (step_ok_tee probes acc)

  and step_ok_probes_wait probes acc recvd =
    let responses,acc = Parser.deserialize (concat_acc acc recvd) in
    try
      (match Probes.responsed probes responses with
       | None   ->
          `Continue (step_ok_probes_wait (Probes.step probes) acc)
       | Some e ->
          Logs.debug (fun m -> m "got probe response: %s"
                               @@ show_event e);
          let probes = Probes.cons e probes in
          if Probes.last probes
          then let probes = Probes.handle pe probes in
               `Continue (step_ok_tee probes acc)
          else let probes = Probes.next probes in
               `Continue (step_ok_probes_send probes acc))
    with Timeout ->
      Logs.warn (fun m -> m "timeout while waiting for probe response, \
                             restarting...");
      first_step ()

  and step_ok_requests_send probes acc _ =
    let probes = Probes.wait probes in
    if Queue.empty !msgs
    then `Continue (step_ok_tee probes acc)
    else (Queue.send !msgs () |> ignore;
          `Continue (step_ok_requests_wait probes acc))

  and step_ok_requests_wait probes acc recvd =
    let probes = Probes.wait probes in
    let responses, acc = Parser.deserialize (concat_acc acc recvd) in
    try
      match Queue.responsed !msgs responses with
      | None -> msgs := Queue.step !msgs;
                `Continue (step_ok_requests_wait probes acc)
      | Some () -> msgs := Queue.next !msgs;
                   `Continue (step_ok_requests_send probes acc)
    with Timeout ->
      Logs.warn (fun m ->
          m "timeout while waiting for client request response, \
             restarting...");
      first_step ()

  in first_step ()

let to_streams_s storage status =
  React.E.map (fun (x : status) ->
      let config : config = storage#get in
      let info =
        let open Stream.Source in
        let scheme = match x.protocol with
          | RTP -> "rtp"
          | UDP -> "udp" in
        match config.ip.multicast with
        | Some x ->
           { addr = x
           ; port = config.ip.port
           ; scheme
           }
        | None ->
           { addr = config.nw.ip
           ; port = config.ip.port
           ; scheme
           } in
      let (stream : Stream.Raw.t) =
        { source = { info = IPV4 info; node = Port 0 }
        ; id = TS_raw
        ; typ = TS
        } in
      if x.asi_bitrate > 0 then [stream] else [])
  @@ React.E.changes ~eq:equal_status status
  |> React.S.hold ~eq:(Util.List.equal Stream.Raw.equal) []

let create logs sender storage step_duration =
  let (module Logs : Logs.LOG) = logs in
  let eq_state = Topology.equal_state in
  let state, state_push = React.S.create ~eq:eq_state `No_response in
  let status, status_push = React.E.create () in
  let config, config_push = React.E.create () in
  let eq_devinfo = Equal.option equal_devinfo in
  let devinfo, devinfo_push = React.S.create ~eq:eq_devinfo None in

  let s_status =
    React.S.hold ~eq:(Equal.option equal_status) None
    @@ React.E.map (fun x -> Some x) status in
  let streams = to_streams_s storage status in
  let events = { streams; state; status; config } in
  let (pe : push_events) =
    { state   = state_push
    ; status  = status_push
    ; config  = config_push
    ; devinfo = devinfo_push
    } in
  let msgs = ref (Queue.create []) in
  let send x = send state msgs sender storage pe
                 (Boards.Timer.steps ~step_duration 2.) x in
  let log n s = Logs.info (fun m -> m "got %s set request: %s" n s) in
  let api =
    { set_ip =
        (fun x ->
          log "ip address" (Ipaddr.V4.to_string x);
          send (Nw (Set_ip x)))
    ; set_mask =
        (fun x ->
          log "network mask" (Ipaddr.V4.to_string x);
          send (Nw (Set_mask x)))
    ; set_gateway =
        (fun x ->
          log "gateway" (Ipaddr.V4.to_string x);
          send (Nw (Set_gateway x)))
    ; set_dhcp =
        (fun x ->
          log "DHCP" (string_of_bool x);
          send (Nw (Set_dhcp x)))
    ; set_enable =
        (fun x ->
          log "enable" (string_of_bool x);
          send (Ip (Set_enable x)))
    ; set_fec =
        (fun x ->
          log "FEC enable" (string_of_bool x);
          send (Ip (Set_fec_enable x)))
    ; set_port =
        (fun x ->
          log "UDP port" (string_of_int x);
          send (Ip (Set_udp_port x)))
    ; set_meth =
        (fun x ->
          log "method" (show_meth x);
          send (Ip (Set_method x)))
    ; set_multicast =
        (fun x ->
          log "multicast" (Ipaddr.V4.to_string x);
          send (Ip (Set_mcast_addr x)))
    ; set_delay =
        (fun x ->
          log "IP-to-output delay" (string_of_int x);
          send (Ip (Set_delay x)))
    ; set_rate_mode =
        (fun x ->
          log "rate estimation mode" (show_rate_mode x);
          send (Ip (Set_rate_est_mode x)))
    ; reset =
        (fun () ->
          Logs.info (fun m -> m "got reset request");
          send (Nw Reboot))
    ; get_status = (fun () -> React.S.value s_status)
    ; get_config = (fun () -> storage#get)
    ; get_devinfo = (fun () -> React.S.value devinfo)
    }
  in
  events,
  api,
  (step msgs sender storage step_duration pe logs)
