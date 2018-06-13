open Containers
open Board_types
open Lwt.Infix
open Storage.Options
open Boards.Board
open Boards.Pools

include Board_parser

(* Board protocol implementation *)

let timeout_period step_duration = 2 * int_of_float (1. /. step_duration) (* 2 secs *)

let request_period step_duration = 2 * int_of_float (1. /. step_duration) (* 5 secs *)

module SM = struct
  
  type push_events = { status : board_status -> unit
                     }

  let wakeup_timeout t = t.pred `Timeout |> ignore

  let send_msg (type a) sender (msg : a request) : unit Lwt.t =
    (match msg with
     | Devinfo _ -> to_req_get msg
     | Overall x -> (match x with
                     | Get_mode          -> to_req_get msg
                     | Get_application   -> to_req_get msg
                     | Get_storage       -> to_req_get msg
                     | Set_mode x        -> to_req_set_int8 msg (mode_to_int x)
                     | Set_application x -> to_req_set_int8 msg (application_to_int x)
                     | Set_storage x     -> to_req_set_int8 msg (storage_to_int x))
     | Nw x      -> (match x with
                     | Get_ip        -> to_req_get msg
                     | Get_mask      -> to_req_get msg
                     | Get_gateway   -> to_req_get msg
                     | Get_dhcp      -> to_req_get msg
                     | Get_mac       -> to_req_get msg
                     | Set_ip x      -> to_req_set_ipaddr msg x
                     | Set_mask x    -> to_req_set_ipaddr msg x
                     | Set_gateway x -> to_req_set_ipaddr msg x
                     | Set_dhcp x    -> to_req_set_bool msg x
                     | Reboot        -> to_req_set_bool msg true)
     | Ip x      -> (match x with
                     | Get_method          -> to_req_get msg
                     | Get_enable          -> to_req_get msg
                     | Get_fec_delay       -> to_req_get msg
                     | Get_fec_enable      -> to_req_get msg
                     | Get_fec_cols        -> to_req_get msg
                     | Get_fec_rows        -> to_req_get msg
                     | Get_jitter_tol      -> to_req_get msg
                     | Get_lost_after_fec  -> to_req_get msg
                     | Get_lost_before_fec -> to_req_get msg
                     | Get_udp_port        -> to_req_get msg
                     | Get_delay           -> to_req_get msg
                     | Get_mcast_addr      -> to_req_get msg
                     | Get_tp_per_ip       -> to_req_get msg
                     | Get_status          -> to_req_get msg
                     | Get_protocol        -> to_req_get msg
                     | Get_output          -> to_req_get msg
                     | Get_packet_size     -> to_req_get msg
                     | Get_bitrate         -> to_req_get msg
                     | Get_pcr_present     -> to_req_get msg
                     | Get_rate_change_cnt -> to_req_get msg
                     | Get_rate_est_mode   -> to_req_get msg
                     | Get_jitter_err_cnt  -> to_req_get msg
                     | Get_lock_err_cnt    -> to_req_get msg
                     | Get_delay_factor    -> to_req_get msg
                     | Set_method x        -> to_req_set_int8 msg (meth_to_int x)
                     | Set_enable x        -> to_req_set_bool msg x
                     | Set_fec_enable x    -> to_req_set_bool msg x
                     | Set_udp_port x      -> to_req_set_int16 msg x
                     | Set_delay x         -> to_req_set_int16 msg x
                     | Set_mcast_addr x    -> to_req_set_ipaddr msg x
                     | Set_rate_est_mode x -> to_req_set_int8 msg (rate_mode_to_int x))
     | Asi x     -> (match x with
                     | Get_packet_size   -> to_req_get msg
                     | Get_bitrate       -> to_req_get msg
                     | Set_packet_size x -> to_req_set_int8 msg (asi_packet_sz_to_int x)))
    |> sender

  let send (type a) msgs sender (storage : config storage) timeout push_config (msg : a request) : a Lwt.t =
    let t, w = Lwt.wait () in
    let pred = function
      | `Timeout -> Lwt.wakeup_exn w (Failure "msg timeout"); None
      | l -> let open Option in
             is_response msg l >|= fun r ->
             let conf = storage#get in
             let f x  = push_config x; storage#store x in
             (match msg with
              | Nw (Set_ip x)            ->
                 let conf = {conf with nw = {conf.nw with ip = x} } in f conf
              | Nw (Set_mask x)          ->
                 let conf = {conf with nw = {conf.nw with mask = x} } in f conf
              | Nw (Set_gateway x)       ->
                 let conf = {conf with nw = {conf.nw with gateway = x} } in f conf
              | Nw (Set_dhcp x)          ->
                 let conf = {conf with nw = {conf.nw with dhcp = x} } in f conf
              | Ip (Set_enable x)        ->
                 let conf = {conf with ip = {conf.ip with enable = x} } in f conf
              | Ip (Set_fec_enable x)    ->
                 let conf = {conf with ip = {conf.ip with fec = x} } in f conf
              | Ip (Set_udp_port x)      ->
                 let conf = {conf with ip = {conf.ip with port = x} } in f conf
              | Ip (Set_mcast_addr x)    ->
                 let conf = {conf with ip = {conf.ip with multicast = Some x} } in f conf
              | Ip (Set_delay x)         ->
                 let conf = {conf with ip = {conf.ip with delay = Some x} } in f conf
              | Ip (Set_rate_est_mode x) ->
                 let conf = {conf with ip = {conf.ip with rate_mode = Some x} } in f conf
              | _ -> ());
             Lwt.wakeup w r in
    let send = fun () -> send_msg sender msg in
    msgs := Queue.append !msgs { send; pred; timeout; exn = None };
    t

  let step msgs sender (storage : config storage) step_duration push_state push_info push_events =

    let period             = timeout_period step_duration in
    let request_period     = request_period step_duration in
    let reboot_steps       = 20 / (int_of_float (step_duration *. (float_of_int period))) in

    let events_to_status ps events =
      let get = fun f e -> Option.get_exn @@ List.find_map f e in
      { fec_delay       = get (function Fec_delay x -> Some x | _ -> None) events
      ; fec_cols        = get (function Fec_cols x -> Some x | _ -> None) events
      ; fec_rows        = get (function Fec_rows x -> Some x | _ -> None) events
      ; jitter_tol      = get (function Jitter_tol x -> Some x | _ -> None) events
      ; lost_after_fec  = (let x = get (function Lost_after_fec x -> Some x | _ -> None) events in
                           match ps with
                           | Some ps -> Int64.(x - ps.lost_after_fec)
                           | None    -> x)
      ; lost_before_fec = (let x = get (function Lost_before_fec x -> Some x | _ -> None) events in
                           match ps with
                           | Some ps -> Int64.(x - ps.lost_before_fec)
                           | None    -> x)
      ; tp_per_ip       = get (function Tp_per_ip x -> Some x | _ -> None) events
      ; status          = get (function Status x -> Some x | _ -> None) events
      ; protocol        = get (function Protocol x -> Some x | _ -> None) events
      ; packet_size     = get (function Packet_size x -> Some x | _ -> None) events
      ; bitrate         = get (function Bitrate x -> Some x | _ -> None) events
      ; pcr_present     = get (function Pcr_present x -> Some x | _ -> None) events
      ; rate_change_cnt = (let x = get (function Rate_change_cnt x -> Some x | _ -> None) events in
                           match ps with
                           | Some ps -> Int32.sub x ps.rate_change_cnt
                           | None    -> x)
      ; jitter_err_cnt  = (let x = get (function Jitter_err_cnt x -> Some x | _ -> None) events in
                           match ps with
                           | Some ps -> Int32.sub x ps.jitter_err_cnt
                           | None    -> x)
      ; lock_err_cnt    = (let x = get (function Lock_err_cnt x -> Some x | _ -> None) events in
                           match ps with
                           | Some ps -> Int32.sub x ps.lock_err_cnt
                           | None    -> x)
      ; delay_factor    = get (function Delay_factor x -> Some x | _ -> None) events
      ; asi_bitrate     = get (function Asi_bitrate x -> Some x | _ -> None) events
      } in

    let find_resp req acc recvd ~success ~failure =
      let responses,acc = deserialize (concat_acc acc recvd) in
      (match List.find_map (is_response req) responses with
       | Some x -> success x acc
       | None   -> failure acc) in

    let send x = send_msg sender x |> ignore in

    let rec first_step () =
      Queue.iter !msgs wakeup_timeout;
      msgs := Queue.create [];
      push_state `No_response;
      let req = Devinfo Get_fpga_ver in
      send_msg sender req |> ignore;
      `Continue (step_detect_fpga_ver period req None)

    and bad_step period next_step = if period < 0 then (first_step ()) else `Continue next_step

    and step_detect_fpga_ver p req acc recvd =
      find_resp req acc recvd
                ~success:(fun x _ -> let r = Devinfo Get_hw_ver in
                                     send r; `Continue (step_detect_hw_ver period r x None))
                ~failure:(fun acc -> bad_step p (step_detect_fpga_ver (pred p) req acc))

    and step_detect_hw_ver p req conf acc recvd =
      find_resp req acc recvd
                ~success:(fun x _ -> let r = Devinfo Get_fw_ver in
                                     send r; `Continue (step_detect_fw_ver period r (conf,x) None))
                ~failure:(fun acc -> bad_step p (step_detect_hw_ver (pred p) req conf acc))

    and step_detect_fw_ver p req ((fpga,hw) as conf) acc recvd =
      find_resp req acc recvd
                ~success:(fun x _ -> let r = Devinfo Get_serial in
                                     send r; `Continue (step_detect_serial period r (fpga,hw,x) None))
                ~failure:(fun acc -> bad_step p (step_detect_fw_ver (pred p) req conf acc))

    and step_detect_serial p req ((fpga,hw,fw) as conf) acc recvd =
      find_resp req acc recvd
                ~success:(fun x _ -> let r = Devinfo Get_type in
                                     send r; `Continue (step_detect_type period r (fpga,hw,fw,x) None))
                ~failure:(fun acc -> bad_step p (step_detect_serial (pred p) req conf acc))

    and step_detect_type p req ((fpga,hw,fw,ser) as conf) acc recvd =
      find_resp req acc recvd
                ~success:(fun x _ -> let r = Nw Get_mac in
                                     send r; `Continue (step_detect_mac period r (fpga,hw,fw,ser,x) None))
                ~failure:(fun acc -> bad_step p (step_detect_type (pred p) req conf acc))

    and step_detect_mac p req conf acc recvd =
      find_resp req acc recvd
                ~success:(fun x _ -> let fpga_ver,hw_ver,fw_ver,serial,typ = conf in
                                     let r = Overall (Set_mode Ip2asi) in
                                     push_info (Some { fpga_ver; hw_ver; fw_ver; serial; typ; mac = x });
                                     push_state `Init;
                                     send r; `Continue (step_init_mode period r None))
                ~failure:(fun acc -> bad_step p (step_detect_mac (pred p) req conf acc))

    and step_detect_after_init ns p steps req acc recvd =
      find_resp req acc recvd
                ~success:(fun _ _ -> `Continue (step_wait_after_detect ns period))
                ~failure:(fun acc -> if p > 0
                                     then `Continue (step_detect_after_init ns (pred p) steps req acc)
                                     else if steps > 0
                                     then (send_msg sender req |> ignore;
                                           `Continue (step_detect_after_init ns period (pred steps) req acc))
                                     else (first_step ()))

    and step_wait_after_detect ns p _ =
      if p > 0 then `Continue (step_wait_after_detect ns (pred p))
      else match ns with
           | `Mode -> let r = Overall (Set_application Normal) in
                      send r; `Continue (step_init_application period r None)
           | `App  -> let r = Overall (Set_storage Ram) in
                      send r; `Continue (step_init_storage period r None)
           | `Nw   -> let r = Ip (Set_enable storage#get.ip.enable) in
                      send r; `Continue (step_init_ip_enable period r None)

    and step_init_mode p req acc recvd =
      find_resp req acc recvd
                ~success:(fun _ _ -> let r = Devinfo Get_fpga_ver in
                                     send r; `Continue (step_detect_after_init `Mode period reboot_steps r None))
                ~failure:(fun acc -> bad_step p (step_init_mode (pred p) req acc))

    and step_init_application p req acc recvd =
      find_resp req acc recvd
                ~success:(fun _ _ -> let r = Devinfo Get_fpga_ver in
                                     send r; `Continue (step_detect_after_init `App period reboot_steps r None))
                ~failure:(fun _ -> bad_step p (step_init_application (pred p) req acc))

    and step_init_storage p req acc recvd =
      find_resp req acc recvd
                ~success:(fun _ _ -> let r = Nw Get_ip in
                                     send r; `Continue (step_get_ip period r None))
                ~failure:(fun acc -> bad_step p (step_init_storage (pred p) req acc))

    and step_get_ip p req acc recvd =
      find_resp req acc recvd
                ~success:(fun resp _ -> if equal_addr resp storage#get.nw.ip
                                        then (let r = Nw Get_mask in
                                              send r; `Continue (step_get_mask false period r None))
                                        else (let r = Nw (Set_ip storage#get.nw.ip) in
                                              send r; `Continue (step_init_ip period r None)))
                ~failure:(fun acc -> bad_step p (step_get_ip (pred p) req acc))

    and step_init_ip p req acc recvd =
      find_resp req acc recvd
                ~success:(fun _ _ -> let r = Nw Get_mask in
                                     send r; `Continue (step_get_mask true period r None))
                ~failure:(fun acc -> bad_step p (step_init_ip (pred p) req acc))

    and step_get_mask need_reboot p req acc recvd =
      find_resp req acc recvd
                ~success:(fun resp _ -> if equal_mask resp storage#get.nw.mask
                                        then (let r = Nw Get_gateway in
                                              send r; `Continue (step_get_gateway need_reboot period r None))
                                        else (let r = Nw (Set_mask storage#get.nw.mask) in
                                              send r; `Continue (step_init_mask period r None)))
                ~failure:(fun acc -> bad_step p (step_get_mask need_reboot (pred p) req acc))

    and step_init_mask p req acc recvd =
      find_resp req acc recvd
                ~success:(fun _ _ -> let r = Nw Get_gateway in
                                     send r; `Continue (step_get_gateway true period r None))
                ~failure:(fun acc -> bad_step p (step_init_mask (pred p) req acc))

    and step_get_gateway need_reboot p req acc recvd =
      find_resp req acc recvd
                ~success:(fun resp _ -> if equal_gateway resp storage#get.nw.gateway
                                        then (let r = Nw Get_dhcp in
                                              send r; `Continue (step_get_dhcp need_reboot period r None))
                                        else (let r = Nw (Set_gateway storage#get.nw.gateway) in
                                              send r; `Continue (step_init_gateway period r None)))
                ~failure:(fun acc -> bad_step p (step_get_gateway need_reboot (pred p) req acc))

    and step_init_gateway p req acc recvd =
      find_resp req acc recvd
                ~success:(fun _ _ -> let r = Nw Get_dhcp in
                                     send r; `Continue (step_get_dhcp true period r None))
                ~failure:(fun acc -> bad_step p (step_init_gateway (pred p) req acc))

    and step_get_dhcp need_reboot p req acc recvd =
      find_resp req acc recvd
                ~success:(fun resp _ -> if Equal.bool resp storage#get.nw.dhcp
                                        then (step_need_reboot need_reboot)
                                        else (let r = Nw (Set_dhcp storage#get.nw.dhcp) in
                                              send r; `Continue (step_init_dhcp period r None)))
                ~failure:(fun acc -> bad_step p (step_get_dhcp need_reboot (pred p) req acc))


    and step_init_dhcp p req acc recvd =
      find_resp req acc recvd
                ~success:(fun _ _ -> step_need_reboot true)
                ~failure:(fun acc -> bad_step p (step_init_dhcp (pred p) req acc))

    and step_need_reboot need_reboot =
      if need_reboot
      then (let r = (Nw Reboot) in
            send r; `Continue (step_finalize_nw_init period r None))
      else (let r = Ip (Set_enable storage#get.ip.enable) in
            send r; `Continue (step_init_ip_enable period r None))

    and step_finalize_nw_init p req acc recvd =
      find_resp req acc recvd
                ~success:(fun _ _ ->
                  let r = Devinfo Get_fpga_ver in
                  send r; `Continue (step_detect_after_init `Nw period reboot_steps r None))
                ~failure:(fun acc -> bad_step p (step_finalize_nw_init (pred p) req acc))

    and step_init_ip_enable p req acc recvd =
      find_resp req acc recvd
                ~success:(fun _ _ -> let r = Ip (Set_fec_enable storage#get.ip.fec) in
                                     send r; `Continue (step_init_ip_fec period r None))
                ~failure:(fun acc -> bad_step p (step_init_ip_enable (pred p) req acc))

    and step_init_ip_fec p req acc recvd =
      find_resp req acc recvd
                ~success:(fun _ _ -> let r = Ip (Set_udp_port storage#get.ip.port) in
                                     send r; `Continue (step_init_ip_udp_port period r None))
                ~failure:(fun acc -> bad_step p (step_init_ip_fec (pred p) req acc))

    and step_init_ip_udp_port p req acc recvd =
      find_resp req acc recvd
                ~success:(fun _ _ -> (match storage#get.ip.multicast with
                                      | Some x -> let r = Ip (Set_mcast_addr x) in
                                                  send r; `Continue (step_init_ip_multicast period r None)
                                      | None   -> let r = Ip (Set_method Unicast) in
                                                  send r; `Continue (step_init_ip_method (pred p) r None)))
                ~failure:(fun acc -> bad_step p (step_init_ip_udp_port (pred p) req acc))

    and step_init_ip_multicast p req acc recvd =
      find_resp req acc recvd
                ~success:(fun _ _ -> let r = Ip (Set_method Multicast) in
                                     send r; `Continue (step_init_ip_method period r None))
                ~failure:(fun acc -> bad_step p (step_init_ip_multicast (pred p) req acc))

    and step_init_ip_method p req acc recvd =
      find_resp req acc recvd
                ~success:(fun _ _ -> let r = Ip (Set_delay (Option.get_or ~default:100 storage#get.ip.delay)) in
                                     send r; `Continue (step_init_ip_delay period r None))
                ~failure:(fun acc -> bad_step p (step_init_ip_method (pred p) req acc))

    and step_init_ip_delay p req acc recvd =
      find_resp req acc recvd
                ~success:(fun _ _ -> let r = Ip (Set_rate_est_mode (Option.get_or ~default:On storage#get.ip.rate_mode)) in
                                     send r; `Continue (step_init_ip_rate_mode period r None))
                ~failure:(fun acc -> bad_step p (step_init_ip_delay (pred p) req acc))

    and step_init_ip_rate_mode p req acc recvd =
      find_resp req acc recvd
                ~success:(fun _ _ -> let msgs = List.map (fun x ->
                                                    { send    = (fun () -> send_msg sender x)
                                                    ; pred    = (is_response x)
                                                    ; timeout = period
                                                    ; exn     = None })
                                                         [ Ip Get_fec_delay
                                                         ; Ip Get_fec_cols
                                                         ; Ip Get_fec_rows
                                                         ; Ip Get_jitter_tol
                                                         ; Ip Get_bitrate
                                                         ; Ip Get_lost_after_fec
                                                         ; Ip Get_lost_before_fec
                                                         ; Ip Get_tp_per_ip
                                                         ; Ip Get_status
                                                         ; Ip Get_protocol
                                                         ; Ip Get_packet_size
                                                         ; Ip Get_pcr_present
                                                         ; Ip Get_rate_change_cnt
                                                         ; Ip Get_jitter_err_cnt
                                                         ; Ip Get_lock_err_cnt
                                                         ; Ip Get_delay_factor
                                                         ; Asi Get_bitrate ] in
                                     let pool = Pool.create msgs in
                                     push_state `Fine;
                                     `Continue (step_normal_probes_send pool None [] 0 None))
                ~failure:(fun acc -> bad_step p (step_init_ip_rate_mode (pred p) req acc))

    and step_normal_probes_send pool prev_status events period_timer acc _ =
      (* if (period_timer >= request_period) then raise (Failure "board ip: sm invariant broken"); *)

      if Pool.empty pool
      then `Continue (step_normal_requests_send pool prev_status (succ period_timer) acc)
      else (Pool.send pool () |> ignore;
            `Continue (step_normal_probes_wait pool prev_status events (succ period_timer) acc))

    and step_normal_probes_wait pool prev_status events period_timer acc recvd =
      let responses,acc = deserialize (concat_acc acc recvd) in

      try
        (match Pool.responsed pool responses with
         | None   -> let pool = Pool.step pool in
                     `Continue (step_normal_probes_wait pool prev_status events (succ period_timer) acc)
         | Some e -> let new_pool = Pool.next pool in
                     if Pool.last pool
                     then (let status = events_to_status prev_status (e :: events) in
                           if Option.is_some prev_status then push_events.status status;
                           `Continue (step_normal_requests_send new_pool (Some status) period_timer acc))
                     else step_normal_probes_send new_pool prev_status (e :: events) period_timer acc recvd)
      with Timeout -> first_step ()

    and step_normal_requests_send probes_pool status period_timer acc _ =
      if (period_timer >= request_period)
      then `Continue (step_normal_probes_send probes_pool status [] ((succ period_timer) mod request_period) acc)
      else
        if Queue.empty !msgs
        then `Continue (step_normal_requests_send probes_pool status (succ period_timer) acc)
        else (Queue.send !msgs () |> ignore;
              `Continue (step_normal_requests_wait probes_pool status (succ period_timer) acc))

    and step_normal_requests_wait probes_pool status period_timer acc recvd =
      let responses, acc = deserialize (concat_acc acc recvd) in
      try
        match Queue.responsed !msgs responses with
        | None    -> msgs := Queue.step !msgs;
                     `Continue (step_normal_requests_wait probes_pool status (succ period_timer) acc)
        | Some () -> msgs := Queue.next !msgs;
                     `Continue (step_normal_requests_send probes_pool status (succ period_timer) acc)
      with Timeout -> first_step ()

    in first_step ()

  let create sender storage push_state step_duration =
    let period = request_period step_duration in
    let status,status_push = React.E.create () in
    let config,push_config = React.E.create () in
    let info,push_info     = React.S.create None in
    let (events : events)  = { status; config } in
    let push_events = { status = status_push } in
    let msgs = ref (Queue.create []) in
    let send x = send msgs sender storage period push_config x in
    let api  = { addr      = (fun x  -> send (Nw (Set_ip x)))
               ; mask      = (fun x  -> send (Nw (Set_mask x)))
               ; gateway   = (fun x  -> send (Nw (Set_gateway x)))
               ; dhcp      = (fun x  -> send (Nw (Set_dhcp x)))
               ; enable    = (fun x  -> send (Ip (Set_enable x)))
               ; fec       = (fun x  -> send (Ip (Set_fec_enable x)))
               ; port      = (fun x  -> send (Ip (Set_udp_port x)))
               ; meth      = (fun x  -> send (Ip (Set_method x)))
               ; multicast = (fun x  -> send (Ip (Set_mcast_addr x)))
               ; delay     = (fun x  -> send (Ip (Set_delay x)))
               ; rate_mode = (fun x  -> send (Ip (Set_rate_est_mode x)))
               ; reset     = (fun () -> send (Nw Reboot))
               ; config    = (fun () -> Lwt.return storage#get)
               ; devinfo   = (fun () -> Lwt.return @@ React.S.value info)
               }
    in
    events,
    api,
    (step msgs sender storage step_duration push_state push_info push_events)

end
