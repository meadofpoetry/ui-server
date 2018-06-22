open Containers
open Common
open Board_types
open Lwt.Infix
open Storage.Options
open Boards.Board
open Boards.Pools

include Board_parser

type events =
  { streams : Stream.stream list React.signal
  ; state   : Topology.state React.signal
  ; status  : board_status React.event
  ; config  : config React.event
  }

type push_events =
  { state   : Topology.state -> unit
  ; status  : board_status -> unit
  ; devinfo : devinfo option -> unit
  ; config  : config  -> unit
  }

type api =
  { post_ip        : addr -> addr Lwt.t
  ; post_mask      : mask -> mask Lwt.t
  ; post_gateway   : gateway -> gateway Lwt.t
  ; post_dhcp      : flag -> flag Lwt.t
  ; post_enable    : flag -> flag Lwt.t
  ; post_fec       : flag -> flag Lwt.t
  ; post_port      : port -> port Lwt.t
  ; post_meth      : meth -> meth Lwt.t
  ; post_multicast : multicast -> multicast Lwt.t
  ; post_delay     : delay -> delay Lwt.t
  ; post_rate_mode : rate_mode -> rate_mode Lwt.t
  ; post_reset     : unit -> unit Lwt.t
  ; get_config     : unit -> config Lwt.t
  ; get_devinfo    : unit -> devinfo option Lwt.t
  }

(* Board protocol implementation *)

let to_period step_duration x = x * int_of_float (1. /. step_duration)

module type M = sig
  val send     : event request -> unit Lwt.t
  val duration : float
  val timeout  : int
end

module type Probes = sig
  type t
  type event_raw = [ `Error of parsed | `Ok of parsed ]
  val handle    : push_events -> t -> t
  val ready     : t -> bool
  val send      : t -> unit Lwt.t
  val cons      : event -> t -> t
  val responsed : t -> event_raw list -> event option
  val next      : t -> t
  val last      : t -> bool
  val wait      : t -> t
  val step      : t -> t
  val make      : unit -> t
end

module Make_probes(M:M) : Probes = struct

  type event_raw = [ `Error of parsed | `Ok of parsed ]
  type pool = (event_raw,event) Pool.t

  let period = to_period M.duration 1

  type t =
    { pool   : pool
    ; prev   : board_status option
    ; timer  : int
    ; events : event list
    }

  let merge ({events;prev;_}:t) : board_status =
    let get = fun f e -> Option.get_exn @@ List.find_map f e in
    { fec_delay       = get (function Fec_delay x -> Some x | _ -> None) events
    ; fec_cols        = get (function Fec_cols x -> Some x | _ -> None) events
    ; fec_rows        = get (function Fec_rows x -> Some x | _ -> None) events
    ; jitter_tol      = get (function Jitter_tol x -> Some x | _ -> None) events
    ; lost_after_fec  = (let x = get (function Lost_after_fec x -> Some x | _ -> None) events in
                         match prev with
                         | Some p -> Int64.(x - p.lost_after_fec)
                         | None   -> x)
    ; lost_before_fec = (let x = get (function Lost_before_fec x -> Some x | _ -> None) events in
                         match prev with
                         | Some p -> Int64.(x - p.lost_before_fec)
                         | None   -> x)
    ; tp_per_ip       = get (function Tp_per_ip x -> Some x | _ -> None) events
    ; status          = get (function Status x -> Some x | _ -> None) events
    ; protocol        = get (function Protocol x -> Some x | _ -> None) events
    ; packet_size     = get (function Packet_size x -> Some x | _ -> None) events
    ; bitrate         = get (function Bitrate x -> Some x | _ -> None) events
    ; pcr_present     = get (function Pcr_present x -> Some x | _ -> None) events
    ; rate_change_cnt = (let x = get (function Rate_change_cnt x -> Some x | _ -> None) events in
                         match prev with
                         | Some p -> Int32.sub x p.rate_change_cnt
                         | None   -> x)
    ; jitter_err_cnt  = (let x = get (function Jitter_err_cnt x -> Some x | _ -> None) events in
                         match prev with
                         | Some p -> Int32.sub x p.jitter_err_cnt
                         | None   -> x)
    ; lock_err_cnt    = (let x = get (function Lock_err_cnt x -> Some x | _ -> None) events in
                         match prev with
                         | Some p -> Int32.sub x p.lock_err_cnt
                         | None   -> x)
    ; delay_factor    = get (function Delay_factor x -> Some x | _ -> None) events
    ; asi_bitrate     = get (function Asi_bitrate x -> Some x | _ -> None) events
    }

  let make_pool () : pool =
    List.map (fun x ->
        { send    = (fun () -> M.send x)
        ; pred    = (is_response x)
        ; timeout = M.timeout
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
             ; Asi Get_bitrate ]
    |> Pool.create

  let responsed (t:t) rsp  = Pool.responsed t.pool rsp
  let ready (t:t)          = not (Pool.empty t.pool || t.timer > 0)
  let send (t:t)           = Pool.send t.pool ()
  let cons (e:event) (t:t) = { t with events = e :: t.events }
  let last (t:t)           = Pool.last t.pool
  let next (t:t)           = { t with pool = Pool.next t.pool }
  let wait (t:t)           = if t.timer <= 0
                             then { t with timer = 0 }
                             else { t with timer = pred t.timer }
  let step (t:t)           = { t with pool = Pool.step t.pool } |> wait
  let handle (pe:push_events) (t:t) : t =
    let status = merge t in
    if Option.is_some t.prev then pe.status status;
    { timer  = period
    ; prev   = Some status
    ; events = List.empty
    ; pool   = Pool.init t.pool
    }
  let make () : t =
    { pool   = make_pool ()
    ; timer  = 0
    ; prev   = None
    ; events = List.empty
    }

end

module SM = struct

  let wakeup_timeout t = t.pred `Timeout |> ignore

  let send_msg (type a) sender (msg : a request) : unit Lwt.t =
    (match msg with
     | Devinfo _ -> to_req_get msg
     | Overall x ->
        begin match x with
        | Get_mode          -> to_req_get msg
        | Get_application   -> to_req_get msg
        | Get_storage       -> to_req_get msg
        | Set_mode x        -> to_req_set_int8 msg (mode_to_int x)
        | Set_application x -> to_req_set_int8 msg (application_to_int x)
        | Set_storage x     -> to_req_set_int8 msg (storage_to_int x)
        end
     | Nw x      ->
        begin match x with
        | Get_ip        -> to_req_get msg
        | Get_mask      -> to_req_get msg
        | Get_gateway   -> to_req_get msg
        | Get_dhcp      -> to_req_get msg
        | Get_mac       -> to_req_get msg
        | Set_ip x      -> to_req_set_ipaddr msg x
        | Set_mask x    -> to_req_set_ipaddr msg x
        | Set_gateway x -> to_req_set_ipaddr msg x
        | Set_dhcp x    -> to_req_set_bool msg x
        | Reboot        -> to_req_set_bool msg true
        end
     | Ip x      ->
        begin match x with
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
        | Set_rate_est_mode x -> to_req_set_int8 msg (rate_mode_to_int x)
        end
     | Asi x     ->
        begin match x with
        | Get_packet_size   -> to_req_get msg
        | Get_bitrate       -> to_req_get msg
        | Set_packet_size x -> to_req_set_int8 msg (asi_packet_sz_to_int x)
        end)
    |> sender

  let send (type a) msgs sender (storage:config storage) (pe:push_events) timeout (msg:a request) : a Lwt.t =
    let t, w = Lwt.wait () in
    let pred = function
      | `Timeout -> Lwt.wakeup_exn w (Failure "msg timeout"); None
      | l -> let open Option in
             is_response msg l >|= fun r ->
             let c = storage#get in
             let c = match msg with
               | Nw (Set_ip _)            -> Some {c with nw = {c.nw with ip        = r }}
               | Nw (Set_mask _)          -> Some {c with nw = {c.nw with mask      = r }}
               | Nw (Set_gateway _)       -> Some {c with nw = {c.nw with gateway   = r }}
               | Nw (Set_dhcp _)          -> Some {c with nw = {c.nw with dhcp      = r }}
               | Ip (Set_enable _)        -> Some {c with ip = {c.ip with enable    = r }}
               | Ip (Set_fec_enable _)    -> Some {c with ip = {c.ip with fec       = r }}
               | Ip (Set_udp_port _)      -> Some {c with ip = {c.ip with port      = r }}
               | Ip (Set_mcast_addr _)    -> Some {c with ip = {c.ip with multicast = Some r }}
               | Ip (Set_delay _)         -> Some {c with ip = {c.ip with delay     = Some r }}
               | Ip (Set_rate_est_mode _) -> Some {c with ip = {c.ip with rate_mode = Some r }}
               | _ -> None
             in
             let () = Option.iter (fun c -> pe.config c; storage#store c) c in
             Lwt.wakeup w r
    in
    let send = fun () -> send_msg sender msg in
    msgs := Queue.append !msgs { send; pred; timeout; exn = None };
    t

  let step msgs sender (storage : config storage) step_duration (pe:push_events) control =
    let period_s = 2 in
    let period       = to_period step_duration period_s in
    let reboot_steps = 20 / (int_of_float (step_duration *. (float_of_int period))) in

    let fmt fmt = let fmt = "(Board IP2TS: %d) " ^^ fmt in Printf.sprintf fmt control in

    let module Probes = Make_probes(struct
                                     let duration = step_duration
                                     let send     = send_msg sender
                                     let timeout  = period
                                   end) in

    let find_resp req acc recvd ~success ~failure =
      let responses,acc = deserialize (concat_acc acc recvd) in
      (match List.find_map (is_response req) responses with
       | Some x -> success x acc
       | None   -> failure acc) in

    let send x = send_msg sender x |> ignore in

    let rec first_step () =
      Logs.info (fun m -> m "%s" (fmt "start of connection establishment..."));
      Queue.iter !msgs wakeup_timeout;
      msgs := Queue.create [];
      pe.state `No_response;
      let req = Devinfo Get_fpga_ver in
      send_msg sender req |> ignore;
      `Continue (step_detect_fpga_ver period req None)

    and bad_step p next_step =
      if p < 0
      then (Logs.warn (fun m ->
                let s = fmt "timeout (%d sec) reached while detect or init step, restarting..." period_s in
                m "%s" s);
            first_step ())
      else `Continue next_step

    and step_detect_fpga_ver p req acc recvd =
      find_resp req acc recvd
                ~success:(fun x _ ->
                  Logs.debug (fun m -> m "%s" (fmt "detect step - got fpga version: %d" x));
                  let r = Devinfo Get_hw_ver in
                  send r; `Continue (step_detect_hw_ver period r x None))
                ~failure:(fun acc -> bad_step p (step_detect_fpga_ver (pred p) req acc))

    and step_detect_hw_ver p req conf acc recvd =
      find_resp req acc recvd
                ~success:(fun x _ ->
                  Logs.debug (fun m -> m "%s" (fmt "detect step - got hw version: %d" x));
                  let r = Devinfo Get_fw_ver in
                  send r; `Continue (step_detect_fw_ver period r (conf,x) None))
                ~failure:(fun acc -> bad_step p (step_detect_hw_ver (pred p) req conf acc))

    and step_detect_fw_ver p req ((fpga,hw) as conf) acc recvd =
      find_resp req acc recvd
                ~success:(fun x _ ->
                  Logs.debug (fun m -> m "%s" (fmt "detect step - got fw version: %d" x));
                  let r = Devinfo Get_serial in
                  send r; `Continue (step_detect_serial period r (fpga,hw,x) None))
                ~failure:(fun acc -> bad_step p (step_detect_fw_ver (pred p) req conf acc))

    and step_detect_serial p req ((fpga,hw,fw) as conf) acc recvd =
      find_resp req acc recvd
                ~success:(fun x _ ->
                  Logs.debug (fun m -> m "%s" (fmt "detect step - got serial: %d" x));
                  let r = Devinfo Get_type in
                  send r; `Continue (step_detect_type period r (fpga,hw,fw,x) None))
                ~failure:(fun acc -> bad_step p (step_detect_serial (pred p) req conf acc))

    and step_detect_type p req ((fpga,hw,fw,ser) as conf) acc recvd =
      find_resp req acc recvd
                ~success:(fun x _ ->
                  Logs.debug (fun m -> m "%s" (fmt "detect step - got type: %d" x));
                  let r = Nw Get_mac in
                  send r; `Continue (step_detect_mac period r (fpga,hw,fw,ser,x) None))
                ~failure:(fun acc -> bad_step p (step_detect_type (pred p) req conf acc))

    and step_detect_mac p req conf acc recvd =
      find_resp req acc recvd
                ~success:(fun mac _ ->
                  Logs.debug (fun m -> m "%s" (fmt "detect step - got mac: %s" @@ Macaddr.to_string mac));
                  let fpga_ver,hw_ver,fw_ver,serial,typ = conf in
                  let r = Overall (Set_mode Ip2asi) in
                  pe.devinfo (Some { fpga_ver; hw_ver; fw_ver; serial; typ; mac });
                  pe.state `Init;
                  Logs.info (fun m -> m "%s" (fmt "connection established, board initialization started..."));
                  send r; `Continue (step_init_mode period r None))
                ~failure:(fun acc -> bad_step p (step_detect_mac (pred p) req conf acc))

    and step_detect_after_init ns p steps req acc recvd =
      find_resp req acc recvd
                ~success:(fun _ _ -> `Continue (step_wait_after_detect ns period))
                ~failure:(fun acc ->
                  if p > 0
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
                ~success:(fun x _ ->
                  Logs.debug (fun m -> m "%s" (fmt "init step - mode set: %s" @@ mode_to_string x));
                  let r = Devinfo Get_fpga_ver in
                  send r; `Continue (step_detect_after_init `Mode period reboot_steps r None))
                ~failure:(fun acc -> bad_step p (step_init_mode (pred p) req acc))

    and step_init_application p req acc recvd =
      find_resp req acc recvd
                ~success:(fun x _ ->
                  Logs.debug (fun m ->
                      let s = (fmt "init step - application set: %s" @@ application_to_string x) in
                      m "%s" s);
                  let r = Devinfo Get_fpga_ver in
                  send r; `Continue (step_detect_after_init `App period reboot_steps r None))
                ~failure:(fun _ -> bad_step p (step_init_application (pred p) req acc))

    and step_init_storage p req acc recvd =
      find_resp req acc recvd
                ~success:(fun x _ ->
                  Logs.debug (fun m ->
                      let s = (fmt "init step - storage set: %s" @@ storage_to_string x) in
                      m "%s" s);
                  let r = Nw Get_ip in
                  send r; `Continue (step_get_ip period r None))
                ~failure:(fun acc -> bad_step p (step_init_storage (pred p) req acc))

    and step_get_ip p req acc recvd =
      find_resp req acc recvd
                ~success:(fun resp _ ->
                  Logs.debug (fun m ->
                      let s = (fmt "init step - got ip: %s" @@ Ipaddr.V4.to_string resp) in
                      m "%s" s);
                  if equal_addr resp storage#get.nw.ip
                  then (let r = Nw Get_mask in
                        Logs.debug (fun m -> m "%s" (fmt "init step - no need to set ip, skipping..."));
                        send r; `Continue (step_get_mask false period r None))
                  else (let r = Nw (Set_ip storage#get.nw.ip) in
                        send r; `Continue (step_init_ip period r None)))
                ~failure:(fun acc -> bad_step p (step_get_ip (pred p) req acc))

    and step_init_ip p req acc recvd =
      find_resp req acc recvd
                ~success:(fun x _ ->
                  Logs.debug (fun m ->
                      let s = (fmt "init step - ip set: %s" @@ Ipaddr.V4.to_string x) in
                      m "%s" s);
                  let r = Nw Get_mask in
                  send r; `Continue (step_get_mask true period r None))
                ~failure:(fun acc -> bad_step p (step_init_ip (pred p) req acc))

    and step_get_mask need_reboot p req acc recvd =
      find_resp req acc recvd
                ~success:(fun resp _ ->
                  Logs.debug (fun m ->
                      let s = (fmt "init step - got mask: %s" @@ Ipaddr.V4.to_string resp) in
                      m "%s" s);
                  if equal_mask resp storage#get.nw.mask
                  then (let r = Nw Get_gateway in
                        Logs.debug (fun m -> m "%s" (fmt "init step - no need to set mask, skipping..."));
                        send r; `Continue (step_get_gateway need_reboot period r None))
                  else (let r = Nw (Set_mask storage#get.nw.mask) in
                        send r; `Continue (step_init_mask period r None)))
                ~failure:(fun acc -> bad_step p (step_get_mask need_reboot (pred p) req acc))

    and step_init_mask p req acc recvd =
      find_resp req acc recvd
                ~success:(fun x _ ->
                  Logs.debug (fun m ->
                      let s = (fmt "init step - mask set: %s" @@ Ipaddr.V4.to_string x) in
                      m "%s" s);
                  let r = Nw Get_gateway in
                  send r; `Continue (step_get_gateway true period r None))
                ~failure:(fun acc -> bad_step p (step_init_mask (pred p) req acc))

    and step_get_gateway need_reboot p req acc recvd =
      find_resp req acc recvd
                ~success:(fun resp _ ->
                  Logs.debug (fun m ->
                      let s = (fmt "init step - got gateway: %s" @@ Ipaddr.V4.to_string resp) in
                      m "%s" s);
                  if equal_gateway resp storage#get.nw.gateway
                  then (let r = Nw Get_dhcp in
                        Logs.debug (fun m -> m "%s" (fmt "init step - no need to set gateway, skipping..."));
                        send r; `Continue (step_get_dhcp need_reboot period r None))
                  else (let r = Nw (Set_gateway storage#get.nw.gateway) in
                        send r; `Continue (step_init_gateway period r None)))
                ~failure:(fun acc -> bad_step p (step_get_gateway need_reboot (pred p) req acc))

    and step_init_gateway p req acc recvd =
      find_resp req acc recvd
                ~success:(fun x _ ->
                  Logs.debug (fun m ->
                      let s = (fmt "init step - gateway set: %s" @@ Ipaddr.V4.to_string x) in
                      m "%s" s);
                  let r = Nw Get_dhcp in
                  send r; `Continue (step_get_dhcp true period r None))
                ~failure:(fun acc -> bad_step p (step_init_gateway (pred p) req acc))

    and step_get_dhcp need_reboot p req acc recvd =
      find_resp req acc recvd
                ~success:(fun resp _ ->
                  Logs.debug (fun m ->
                      let s = (fmt "init step - got DHCP: %s" @@ string_of_bool resp) in
                      m "%s" s);
                  if Equal.bool resp storage#get.nw.dhcp
                  then
                    (Logs.debug (fun m -> m "%s" (fmt "init step - no need to set DHCP, skipping..."));
                     step_need_reboot need_reboot)
                  else (let r = Nw (Set_dhcp storage#get.nw.dhcp) in
                        send r; `Continue (step_init_dhcp period r None)))
                ~failure:(fun acc -> bad_step p (step_get_dhcp need_reboot (pred p) req acc))


    and step_init_dhcp p req acc recvd =
      find_resp req acc recvd
                ~success:(fun x _ ->
                  Logs.debug (fun m ->
                      let s = (fmt "init step - DHCP set: %s" @@ string_of_bool x) in
                      m "%s" s);
                  step_need_reboot true)
                ~failure:(fun acc -> bad_step p (step_init_dhcp (pred p) req acc))

    and step_need_reboot need_reboot =
      if need_reboot
      then
        (Logs.debug (fun m -> m "%s" (fmt "init step - rebooting..."));
         let r = (Nw Reboot) in
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
                ~success:(fun x _ ->
                  Logs.debug (fun m ->
                      let s = (fmt "init step - ip enable set: %s" @@ string_of_bool x) in
                      m "%s" s);
                  let r = Ip (Set_fec_enable storage#get.ip.fec) in
                  send r; `Continue (step_init_ip_fec period r None))
                ~failure:(fun acc -> bad_step p (step_init_ip_enable (pred p) req acc))

    and step_init_ip_fec p req acc recvd =
      find_resp req acc recvd
                ~success:(fun x _ ->
                  Logs.debug (fun m ->
                      let s = (fmt "init step - fec enable set: %s" @@ string_of_bool x) in
                      m "%s" s);
                  let r = Ip (Set_udp_port storage#get.ip.port) in
                  send r; `Continue (step_init_ip_udp_port period r None))
                ~failure:(fun acc -> bad_step p (step_init_ip_fec (pred p) req acc))

    and step_init_ip_udp_port p req acc recvd =
      find_resp req acc recvd
                ~success:(fun x _ ->
                  Logs.debug (fun m ->
                      let s = (fmt "init step - udp port set: %s" @@ string_of_int x) in
                      m "%s" s);
                  (match storage#get.ip.multicast with
                   | Some x -> let r = Ip (Set_mcast_addr x) in
                               send r; `Continue (step_init_ip_multicast period r None)
                   | None   -> let r = Ip (Set_method Unicast) in
                               send r; `Continue (step_init_ip_method (pred p) r None)))
                ~failure:(fun acc -> bad_step p (step_init_ip_udp_port (pred p) req acc))

    and step_init_ip_multicast p req acc recvd =
      find_resp req acc recvd
                ~success:(fun x _ ->
                  Logs.debug (fun m ->
                      let s = (fmt "init step - multicast address set: %s" @@ Ipaddr.V4.to_string x) in
                      m "%s" s);
                  let r = Ip (Set_method Multicast) in
                  send r; `Continue (step_init_ip_method period r None))
                ~failure:(fun acc -> bad_step p (step_init_ip_multicast (pred p) req acc))

    and step_init_ip_method p req acc recvd =
      find_resp req acc recvd
                ~success:(fun x _ ->
                  Logs.debug (fun m ->
                      let s = (fmt "init step - ip method set: %s" @@ meth_to_string x) in
                      m "%s" s);
                  let r = Ip (Set_delay (Option.get_or ~default:100 storage#get.ip.delay)) in
                  send r; `Continue (step_init_ip_delay period r None))
                ~failure:(fun acc -> bad_step p (step_init_ip_method (pred p) req acc))

    and step_init_ip_delay p req acc recvd =
      find_resp req acc recvd
                ~success:(fun x _ ->
                  Logs.debug (fun m ->
                      let s = (fmt "init step - ip delay set: %s" @@ string_of_int x) in
                      m "%s" s);
                  let r = Ip (Set_rate_est_mode (Option.get_or ~default:On storage#get.ip.rate_mode)) in
                  send r; `Continue (step_init_ip_rate_mode period r None))
                ~failure:(fun acc -> bad_step p (step_init_ip_delay (pred p) req acc))

    and step_init_ip_rate_mode p req acc recvd =
      find_resp req acc recvd
                ~success:(fun x _ ->
                  Logs.debug (fun m ->
                      let s = (fmt "init step - ip rate mode set: %s" @@ rate_mode_to_string x) in
                      m "%s" s);
                  pe.state `Fine;
                  Logs.info (fun m -> m "%s" (fmt "initialization done!"));
                  let probes = Probes.make () in
                  `Continue (step_ok_probes_send probes None))
                ~failure:(fun acc -> bad_step p (step_init_ip_rate_mode (pred p) req acc))

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
      let responses,acc = deserialize (concat_acc acc recvd) in
      try
        (match Probes.responsed probes responses with
         | None   ->
            `Continue (step_ok_probes_wait (Probes.step probes) acc)
         | Some e ->
            Logs.debug (fun m -> m "%s" @@ fmt "got probe response: %s" @@ show_event e);
            let probes = Probes.cons e probes in
            if Probes.last probes
            then let probes = Probes.handle pe probes in
                 `Continue (step_ok_tee probes acc)
            else let probes = Probes.next probes in
                 `Continue (step_ok_probes_send probes acc))
      with Timeout ->
        Logs.warn (fun m -> m "%s" @@ fmt "timeout while waiting for probe response, restarting...");
        first_step ()

    and step_ok_requests_send probes acc _ =
      let probes = Probes.wait probes in
      if Queue.empty !msgs
      then `Continue (step_ok_tee probes acc)
      else (Queue.send !msgs () |> ignore;
            `Continue (step_ok_requests_wait probes acc))

    and step_ok_requests_wait probes acc recvd =
      let probes = Probes.wait probes in
      let responses, acc = deserialize (concat_acc acc recvd) in
      try
        match Queue.responsed !msgs responses with
        | None    -> msgs := Queue.step !msgs;
                     `Continue (step_ok_requests_wait probes acc)
        | Some () -> msgs := Queue.next !msgs;
                     `Continue (step_ok_requests_send probes acc)
      with Timeout ->
        Logs.warn (fun m -> m "%s" @@ fmt "timeout while waiting for client request response, restarting...");
        first_step ()

    in first_step ()

  let to_streams_s status =
    React.E.map (fun (x:board_status) ->
        let (stream:Common.Stream.stream) =
          { source      = Port 0
          ; id          = `Ts Single
          ; description = Some ""
          }
        in
        if x.asi_bitrate > 0 then List.pure stream else List.empty)
    @@ React.E.changes status
    |> React.S.hold []

  let create control sender storage step_duration =
    let state,state_push     = React.S.create `No_response in
    let status,status_push   = React.E.create () in
    let config,config_push   = React.E.create () in
    let devinfo,devinfo_push = React.S.create None in
    let streams              = to_streams_s status in
    let (events:events)      = { streams; state; status; config } in
    let (pe:push_events) =
      { state   = state_push
      ; status  = status_push
      ; config  = config_push
      ; devinfo = devinfo_push
      }
    in
    let msgs   = ref (Queue.create []) in
    let send x = send msgs sender storage pe (to_period step_duration 2) x in
    let api =
      { post_ip        = (fun x  -> send (Nw (Set_ip x)))
      ; post_mask      = (fun x  -> send (Nw (Set_mask x)))
      ; post_gateway   = (fun x  -> send (Nw (Set_gateway x)))
      ; post_dhcp      = (fun x  -> send (Nw (Set_dhcp x)))
      ; post_enable    = (fun x  -> send (Ip (Set_enable x)))
      ; post_fec       = (fun x  -> send (Ip (Set_fec_enable x)))
      ; post_port      = (fun x  -> send (Ip (Set_udp_port x)))
      ; post_meth      = (fun x  -> send (Ip (Set_method x)))
      ; post_multicast = (fun x  -> send (Ip (Set_mcast_addr x)))
      ; post_delay     = (fun x  -> send (Ip (Set_delay x)))
      ; post_rate_mode = (fun x  -> send (Ip (Set_rate_est_mode x)))
      ; post_reset     = (fun () -> send (Nw Reboot))
      ; get_config     = (fun () -> Lwt.return storage#get)
      ; get_devinfo    = (fun () -> Lwt.return @@ React.S.value devinfo)
      }
    in
    events,
    api,
    (step msgs sender storage step_duration pe control)

end
