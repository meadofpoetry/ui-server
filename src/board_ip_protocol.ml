open Common.Board.Ip
open Lwt.Infix
open Board_meta

include Board_ip_parser

[@@@ocaml.warning "-26"]

let io x = Lwt_io.printf "%s\n" x |> ignore

(* Board protocol implementation *)

let timeout_period step_duration = 2 * int_of_float (1. /. step_duration) (* 2 secs *)

let request_period step_duration = 5 * int_of_float (1. /. step_duration) (* 5 secs *)

module SM = struct

  type event = [ `Ip of (int * rw * Cbuffer.t) ]

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

  let send_event (type a) sender (msg : a event_request) : unit Lwt.t =
    (match msg with
     | _ -> to_req_get_event msg)
    |> sender

  let send msgs sender msg =
    let t, w = Lwt.wait () in
    let pred = function
      | `Timeout -> Lwt.wakeup_exn w (Failure "msg timeout"); None
      | l -> CCOpt.( is_response msg l >|= Lwt.wakeup w ) in
    let send = fun () -> send_msg sender msg in
    msgs := Msg_queue.append !msgs { send; pred };
    t

  let step msgs sender step_duration push_state =

    let period = timeout_period step_duration in
    let reboot_steps = 20 / (int_of_float (step_duration *. (float_of_int period))) in

    let find_resp req acc recvd ~success ~failure =
      let responses,acc = deserialize (Board_meta.concat_acc acc recvd) in
      (match CCList.find_map (is_response req) responses with
       | Some x -> success x acc
       | None   -> failure acc) in

    let send x = send_msg sender x |> ignore in

    let rec first_step () =
      let req       = Devinfo Get_fpga_ver in
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
                                     let conf = { fpga_ver; hw_ver; fw_ver; serial; typ; mac = x } in
                                     io (devinfo_to_yojson conf |> Yojson.Safe.to_string);
                                     let r = Overall (Set_mode Ip2asi) in
                                     send r; `Continue (step_init_mode period r None))
                ~failure:(fun acc -> bad_step p (step_detect_mac (pred p) req conf acc))

    and step_detect_after_init ns p steps req acc recvd =
      find_resp req acc recvd
                ~success:(fun _ _ ->
                  (match ns with
                   | `Mode -> let r = Overall (Set_application Normal) in
                              send_msg sender r |> ignore;
                              `Continue (step_init_application period r None)
                   | `App  -> let r = Overall (Set_storage Ram) in
                              send_msg sender r |> ignore;
                              `Continue (step_init_storage period r None)
                   | `Nw   -> let msgs = List.map (fun x -> { send = (fun () -> send_event sender x)
                                                            ; pred = (is_event x)})
                                                  [ Get_fec_delay
                                                  ; Get_fec_cols
                                                  ; Get_fec_rows
                                                  ; Get_jitter_tol
                                                  ; Get_lost_after_fec
                                                  ; Get_lost_before_fec
                                                  ; Get_tp_per_ip
                                                  ; Get_status
                                                  ; Get_protocol
                                                  ; Get_packet_size
                                                  ; Get_bitrate
                                                  ; Get_pcr_present
                                                  ; Get_rate_change_cnt
                                                  ; Get_jitter_err_cnt
                                                  ; Get_lock_err_cnt
                                                  ; Get_delay_factor] in
                              let pool = Msg_pool.create period msgs in
                              `Continue (step_normal_start pool 0 None)))
                ~failure:(fun acc -> if p > 0
                                     then `Continue (step_detect_after_init ns (pred p) steps req acc)
                                     else if steps > 0
                                     then (send_msg sender req |> ignore;
                                           `Continue (step_detect_after_init ns period (pred steps) req acc))
                                     else (first_step ()))

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
                ~success:(fun _ _ -> `Continue (step_start_init_nw ()))
                ~failure:(fun acc -> bad_step p (step_init_storage (pred p) req acc))

    and step_start_init_nw () =
      let settings = { ip      = Ipaddr.V4.of_string_exn "192.168.111.68"
                     ; mask    = Ipaddr.V4.of_string_exn "255.255.255.0"
                     ; gateway = Ipaddr.V4.of_string_exn "192.168.111.1"
                     ; dhcp    = false
                     } in
      let nw_msgs  = List.map (fun x -> { send = (fun () -> send_msg sender x)
                                        ; pred = (is_response x)})
                              [ Nw (Set_ip settings.ip)
                              ; Nw (Set_mask settings.mask)
                              ; Nw (Set_gateway settings.gateway)] in
      let dhcp_msg = Nw (Set_dhcp settings.dhcp) in
      let init_pool = Msg_pool.create period nw_msgs in
      send_msg sender dhcp_msg |> ignore;
      step_init_dhcp period dhcp_msg init_pool None

    and step_init_dhcp p req init_pool acc recvd =
      find_resp req acc recvd
                ~success:(fun _ _ -> Msg_pool.send init_pool () |> ignore;
                                     `Continue (step_init_nw init_pool None))
                ~failure:(fun acc -> bad_step p (step_init_dhcp (pred p) req init_pool acc))

    and step_init_nw init_pool acc recvd =
      io "step init nw";
      let responses,acc = deserialize (Board_meta.concat_acc acc recvd) in
      match Msg_pool.responsed init_pool responses with
      | None   -> if init_pool.timer < 0 then (first_step ())
                  else `Continue (step_init_nw (Msg_pool.step init_pool) acc)
      | Some _ ->
         match Msg_pool.last init_pool with
         | true -> let r = (Nw Reboot) in
                   send_msg sender r |> ignore;
                   `Continue (step_finalize_init period r None)
         | false -> let init_pool = Msg_pool.next init_pool in
                    Msg_pool.send init_pool () |> ignore;
                    `Continue (step_init_nw init_pool acc)

    and step_finalize_init p req acc recvd =
      find_resp req acc recvd
                ~success:(fun _ _ -> let r = Devinfo Get_fpga_ver in
                                     send r; `Continue (step_detect_after_init `Nw period reboot_steps r None))
                ~failure:(fun acc -> bad_step p (step_finalize_init (pred p) req acc))

    and step_normal_start probes_pool period_timer acc _ =
      io "normal step start";
      if not @@ Msg_pool.empty probes_pool
      then Msg_pool.send probes_pool () |> ignore;
      `Continue (step_normal_probes probes_pool (succ period_timer) acc)

    and step_normal_probes probes_pool period_timer acc recvd =
      let responses,acc = deserialize (Board_meta.concat_acc acc recvd) in
      try
        let sent,probes_pool = if Msg_pool.empty probes_pool
                               then false, probes_pool
                               else (match Msg_pool.responsed probes_pool events with
                                     | None    -> false, Msg_pool.step probes_pool
                                     | Some () -> let probes_pool = Msg_pool.next probes_pool in
                                                  Msg_pool.send probes_pool () |> ignore;
                                                  true, probes_pool)
        in
        CCList.iter push_events responses;
        if Msg_pool.last probes_pool
        then `Continue (step_normal_probes_cleanup sent probes_pool (succ period_timer) acc)
        else `Continue (step_normal_probes probes_pool (succ period_timer) acc)
      with Timeout -> first_step ()

    and step_normal_probes_cleanup sent probes_pool period_timer acc recvd =
      io "normal step probes cleanup";
      if not sent
      then (step_normal_requests (Msg_pool.next probes_pool) period_timer acc recvd)
      else
        let responses,acc = deserialize (Board_meta.concat_acc acc recvd) in
        try
          let recvd = if Msg_pool.empty probes_pool
                      then true
                      else (match Msg_pool.responsed probes_pool responses with
                            | None   -> false
                            | Some _ -> true)
          in
          CCList.iter push_events responses;
          if recvd
          then let probes_pool = (Msg_pool.next probes_pool) in
               if (period_timer > request_period)
               then raise (Failure "board_ip sm invariant broken")
               else `Continue (step_normal_requests probes_pool (succ period_timer) acc)
          else `Continue (step_normal_probes_cleanup sent probes_pool (succ period_timer) acc)
        with Timeout -> first_step ()

    and step_normal_requests probes_pool period_timer acc recvd =
      let responses,acc = deserialize (Board_meta.concat_acc acc recvd) in
      try
        let sent =
          if not @@ Msg_queue.empty !msgs
          then begin match Msg_queue.responded !msgs responses with
               | None -> msgs := Msg_queue.step !msgs; false
               | Some () -> msgs := Msg_queue.next !msgs;
                            Msg_queue.send !msgs () |> ignore;
                            true
               end
          else false
        in
        if (period_timer >= request_period)
        then `Continue (step_normal_requests_clenup sent probes_pool (succ period_timer) acc)
        else `Continue (step_normal_requests probes_pool (succ period_timer) acc)
      with Timeout -> first_step ()

    and step_normal_requests_cleanup sent probes_pool period_timer acc recvd =
      io "normal step requests cleanup";
      if not sent
      then step_normal_start probes_pool ((succ period_timer) mod request_period) acc recvd
      else
        let responses,acc = deserialize (Board_meta.concat acc recvd) in
        try
          let recvd =
            match Msg_queue.responded !msgs responses with
            | None    -> msgs := Msg_queue.step !msgs; false
            | Some () -> msgs := Msg_queue.next !msgs; true
          in
          if recvd
          then `Continue (step_normal_start probes_pool ((succ_period_timer) mod request_period) acc)
          else `Continue (step_normal_requests_cleanup sent probes_pool (succ period_timer) acc)
        with Timeout -> first_step ()

  let create sender push_state step_duration =
    let period = request_period step_duration in
    let msgs = ref (Msg_queue.create period []) in
    (send msgs sender),
    (step msgs sender step_duration push_state)
    

end
