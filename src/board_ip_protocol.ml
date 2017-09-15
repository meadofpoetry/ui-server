open Common.Board.Ip
open Lwt.Infix
open Board_meta

include Board_ip_parser


let io x = Lwt_io.printf "%s\n" x |> ignore

(* Board protocol implementation *)

let period = 50

let init  = [ Overall (Set_mode Ip2asi) ]

let init_msgs (send_req : 'a request -> unit Lwt.t) =
  List.map (fun x -> { send = (fun () -> send_req x)
                     ; pred = (is_response x)})
           init

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

  let send msgs sender msg =
    let t, w = Lwt.wait () in
    let pred = function
      | `Timeout -> Lwt.wakeup_exn w (Failure "msg timeout"); None
      | l -> CCOpt.( is_response msg l >|= Lwt.wakeup w ) in
    let send = fun () -> send_msg sender msg in
    msgs := Msg_queue.append !msgs { send; pred };
    t

  let initial_timeout = -1

  let step msgs sender push_state =

    let rec first_step () =
      let req       = Devinfo Get_fpga_ver in
      send_msg sender req |> ignore;
      `Continue (step_detect_fpga_ver period req None)

    and step_detect_fpga_ver p req acc recvd =
      let responses,acc = deserialize (Board_meta.concat_acc acc recvd) in
      match CCList.find_map (is_response req) responses with
      | Some x -> let r = (Devinfo Get_hw_ver) in
                  send_msg sender r |> ignore;
                  `Continue (step_detect_hw_ver period r x None)
      | None   -> if p > 0
                  then `Continue (step_detect_fpga_ver (pred p) req acc)
                  else (first_step ())

    and step_detect_hw_ver p req conf acc recvd =
      let responses,acc = deserialize (Board_meta.concat_acc acc recvd) in
      match CCList.find_map (is_response req) responses with
      | Some x -> let r = (Devinfo Get_fw_ver) in
                  send_msg sender r |> ignore;
                  `Continue (step_detect_fw_ver period r (conf,x) None)
      | None   -> if p > 0 then `Continue (step_detect_hw_ver (pred p) req conf acc) else (first_step ())

    and step_detect_fw_ver p req ((fpga,hw) as conf) acc recvd =
      let responses,acc = deserialize (Board_meta.concat_acc acc recvd) in
      match CCList.find_map (is_response req) responses with
      | Some x -> let r =  (Devinfo Get_serial) in
                  send_msg sender r |> ignore;
                  `Continue (step_detect_serial period r (fpga,hw,x) None)
      | None   -> if p > 0 then `Continue (step_detect_fw_ver (pred p) req conf acc) else (first_step ())

    and step_detect_serial p req ((fpga,hw,fw) as conf) acc recvd =
      let responses,acc = deserialize (Board_meta.concat_acc acc recvd) in
      match CCList.find_map (is_response req) responses with
      | Some x -> let r = (Devinfo Get_type) in
                  send_msg sender r |> ignore;
                  `Continue (step_detect_type period r (fpga,hw,fw,x) None)
      | None   -> if p > 0 then `Continue (step_detect_serial (pred p) req conf acc) else (first_step ())

    and step_detect_type p req ((fpga,hw,fw,ser) as conf) acc recvd =
      let responses,acc = deserialize (Board_meta.concat_acc acc recvd) in
      match CCList.find_map (is_response req) responses with
      | Some x -> let r = (Nw Get_mac) in
                  send_msg sender r |> ignore;
                  `Continue (step_detect_mac period r (fpga,hw,fw,ser,x) None)
      | None   -> if p > 0 then `Continue (step_detect_type (pred p) req conf acc) else (first_step ())

    and step_detect_mac p req conf acc recvd =
      let responses,acc = deserialize (Board_meta.concat_acc acc recvd) in
      match CCList.find_map (is_response req) responses with
      | Some x -> let fpga_ver,hw_ver,fw_ver,serial,typ = conf in
                  let conf = { fpga_ver; hw_ver; fw_ver; serial; typ; mac = x } in
                  io (devinfo_to_yojson conf |> Yojson.Safe.to_string);
                  (step_start_init ())
      | None   -> if p > 0 then `Continue (step_detect_mac (pred p) req conf acc) else (first_step ())

    and step_start_init () =
      let req = (Overall (Set_mode Asi2ip)) in
      send_msg sender req |> ignore;
      `Continue (step_normal None)

    (* and step_init_mode req acc recvd = *)
    (*   Lwt_io.printf "Init step\n" |> ignore; *)
    (*   let recvd = Board_meta.concat_acc acc recvd in *)
    (*   let responses, acc = deserialize recvd in *)
    (*   match Msg_pool.responsed init_pool responses with *)
    (*   | None   -> if init_pool.timer < 0 *)
    (*               then (first_step ()) *)
    (*               else `Continue (step_init (Msg_pool.step init_pool) acc) *)
    (*   | Some _ -> *)
    (*      match Msg_pool.last init_pool with *)
    (*      | true -> push_state `Fine; *)
    (*                `Continue (step_normal acc) *)
    (*      | false -> let init_pool = Msg_pool.next init_pool in *)
    (*                 Msg_pool.send init_pool () |> ignore; *)
    (*                 `Continue (step_init init_pool acc) *)

    and step_normal acc recvd =
      Lwt_io.printf "Normal step\n" |> ignore;
      let recvd = Board_meta.concat_acc acc recvd in
      let responses, acc = deserialize recvd in

      try
        if not @@ Msg_queue.empty !msgs
        then begin match Msg_queue.responsed !msgs responses with
             | None    -> msgs := Msg_queue.step !msgs
             | Some () -> msgs := Msg_queue.next !msgs;
                          Msg_queue.send !msgs () |> ignore
             end;
        (* let probes_pool = if Msg_pool.empty probes_pool *)
        (*                   then probes_pool *)
        (*                   else (match Msg_pool.responsed probes_pool events with *)
        (*                         | None -> Msg_pool.step probes_pool *)
        (*                         | Some -> let probes_pool = Msg_pool.next probes_pool in *)
        (*                                   Msg_pool.send probes_pool () |> ignore; *)
        (*                                   probes_pool) *)
        (* in *)
        (* CCList.iter push_events events; *)
        `Continue (step_normal acc)
      with
      | Timeout -> Msg_queue.iter !msgs wakeup_timeout;
                   msgs := Msg_queue.create period [];
                   push_state `No_response;
                   (first_step ())
    in
    first_step ()

  let create sender push_state =
    let msgs = ref (Msg_queue.create period []) in
    (send msgs sender),
    (step msgs sender push_state)
    

end
