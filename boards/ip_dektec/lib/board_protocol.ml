open Containers
open Common
open Board_types
open Lwt.Infix
open Storage.Options
open Boards
open Boards.Board
open Boards.Pools
open Board_parser

type events =
  { streams : Stream.stream list React.signal
  ; state   : Topology.state React.signal
  ; status  : status React.event
  ; config  : config React.event
  }

type push_events =
  { state   : Topology.state -> unit
  ; status  : status -> unit
  ; devinfo : devinfo option -> unit
  ; config  : config  -> unit
  }

type api =
  { set_ip        : Ipaddr.V4.t -> Ipaddr.V4.t Lwt.t
  ; set_mask      : Ipaddr.V4.t -> Ipaddr.V4.t Lwt.t
  ; set_gateway   : Ipaddr.V4.t -> Ipaddr.V4.t Lwt.t
  ; set_dhcp      : bool -> bool Lwt.t
  ; set_enable    : bool -> bool Lwt.t
  ; set_fec       : bool -> bool Lwt.t
  ; set_port      : int  -> int Lwt.t
  ; set_meth      : meth -> meth Lwt.t
  ; set_multicast : Ipaddr.V4.t -> Ipaddr.V4.t Lwt.t
  ; set_delay     : int  -> int Lwt.t
  ; set_rate_mode : rate_mode -> rate_mode Lwt.t
  ; reset         : unit -> unit Lwt.t
  ; get_status    : unit -> status option
  ; get_config    : unit -> config
  ; get_devinfo   : unit -> devinfo option
  }

(* Board protocol implementation *)

let timeout = 3 (* seconds *)

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

  let period = Boards.Timer.steps ~step_duration:M.duration 1

  type t =
    { pool   : pool
    ; prev   : status option
    ; timer  : int
    ; events : event list
    }

  let merge ({events;prev;_}:t) : status =
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
    let open Request in
    let buf = match msg with
      | Devinfo _ -> make_get msg
      | Overall x ->
         begin match x with
         | Get_mode          -> make_get msg
         | Get_application   -> make_get msg
         | Get_storage       -> make_get msg
         | Set_mode x        -> Set.int8 msg (mode_to_int x)
         | Set_application x -> Set.int8 msg (application_to_int x)
         | Set_storage x     -> Set.int8 msg (storage_to_int x)
         end
      | Nw x      ->
         begin match x with
         | Get_ip        -> make_get msg
         | Get_mask      -> make_get msg
         | Get_gateway   -> make_get msg
         | Get_dhcp      -> make_get msg
         | Get_mac       -> make_get msg
         | Set_ip x      -> Set.ipaddr msg x
         | Set_mask x    -> Set.ipaddr msg x
         | Set_gateway x -> Set.ipaddr msg x
         | Set_dhcp x    -> Set.bool msg x
         | Reboot        -> Set.bool msg true
         end
      | Ip x      ->
         begin match x with
         | Get_method          -> make_get msg
         | Get_enable          -> make_get msg
         | Get_fec_delay       -> make_get msg
         | Get_fec_enable      -> make_get msg
         | Get_fec_cols        -> make_get msg
         | Get_fec_rows        -> make_get msg
         | Get_jitter_tol      -> make_get msg
         | Get_lost_after_fec  -> make_get msg
         | Get_lost_before_fec -> make_get msg
         | Get_udp_port        -> make_get msg
         | Get_delay           -> make_get msg
         | Get_mcast_addr      -> make_get msg
         | Get_tp_per_ip       -> make_get msg
         | Get_status          -> make_get msg
         | Get_protocol        -> make_get msg
         | Get_output          -> make_get msg
         | Get_packet_size     -> make_get msg
         | Get_bitrate         -> make_get msg
         | Get_pcr_present     -> make_get msg
         | Get_rate_change_cnt -> make_get msg
         | Get_rate_est_mode   -> make_get msg
         | Get_jitter_err_cnt  -> make_get msg
         | Get_lock_err_cnt    -> make_get msg
         | Get_delay_factor    -> make_get msg
         | Set_method x        -> Set.int8 msg (meth_to_int x)
         | Set_enable x        -> Set.bool msg x
         | Set_fec_enable x    -> Set.bool msg x
         | Set_udp_port x      -> Set.int16 msg x
         | Set_delay x         -> Set.int16 msg x
         | Set_mcast_addr x    -> Set.ipaddr msg x
         | Set_rate_est_mode x -> Set.int8 msg (rate_mode_to_int x)
         end
      | Asi x     ->
         begin match x with
         | Get_packet_size   -> make_get msg
         | Get_bitrate       -> make_get msg
         | Set_packet_size x -> Set.int8 msg (asi_packet_sz_to_int x)
         end
    in sender buf

  let send (type a) state msgs sender (storage:config storage) (pe:push_events)
        timeout (msg:a request) : a Lwt.t =
    match React.S.value state with
    | `Fine ->
       let t, w = Lwt.wait () in
       let pred = function
         | `Timeout -> Lwt.wakeup_exn w (Failure "msg timeout"); None
         | l -> let open Option in
                is_response msg l >|= fun r ->
                (* FIXME previous value in response, investigate why *)
                let c = storage#get in
                let c = match msg with
                  | Nw (Set_ip x)            -> Some {c with nw = {c.nw with ip        = x }}
                  | Nw (Set_mask x)          -> Some {c with nw = {c.nw with mask      = x }}
                  | Nw (Set_gateway x)       -> Some {c with nw = {c.nw with gateway   = x }}
                  | Nw (Set_dhcp x)          -> Some {c with nw = {c.nw with dhcp      = x }}
                  | Ip (Set_enable x)        -> Some {c with ip = {c.ip with enable    = x }}
                  | Ip (Set_fec_enable x)    -> Some {c with ip = {c.ip with fec       = x }}
                  | Ip (Set_udp_port x)      -> Some {c with ip = {c.ip with port      = x }}
                  | Ip (Set_method x)        -> (match x with
                                                 | Unicast   -> Some {c with ip = {c.ip with multicast = None }}
                                                 | Multicast -> None)
                  | Ip (Set_mcast_addr x)    -> Some {c with ip = {c.ip with multicast = Some x }}
                  | Ip (Set_delay x)         -> Some {c with ip = {c.ip with delay     = x }}
                  | Ip (Set_rate_est_mode x) -> Some {c with ip = {c.ip with rate_mode = x }}
                  | _ -> None
                in
                let () = Option.iter (fun c -> pe.config c; storage#store c) c in
                Lwt.wakeup w r
       in
       let send = fun () -> send_msg sender msg in
       msgs := Queue.append !msgs { send; pred; timeout; exn = None };
       t
    | _ -> Lwt.fail (Failure "board is not responding")

  let step msgs sender (storage:config storage) step_duration (pe:push_events) log_prefix =
    let reboot_steps = 7 in

    let fmt fmt = let fs = "%s" ^^ fmt in Printf.sprintf fs log_prefix in

    let module Parser = Board_parser.Make(struct let log_prefix = log_prefix end) in
    let module Probes = Make_probes(struct
                            let duration = step_duration
                            let send     = send_msg sender
                            let timeout  = Timer.steps ~step_duration timeout
                          end) in

    let send x = send_msg sender x |> ignore in

    let find_resp (type a) (timer:Timer.t) (req:a request) acc recvd ~on_success ~on_failure ~on_timeout =
      try
        let timer = Timer.step timer in
        let responses,acc = Parser.deserialize (concat_acc acc recvd) in
        match List.find_map (is_response req) responses with
        | Some x -> on_success x acc
        | None   -> on_failure timer acc
      with Timer.Timeout t -> on_timeout t acc in

    let rec first_step () =
      Logs.info (fun m -> m "%s" (fmt "start of connection establishment..."));
      Queue.iter !msgs wakeup_timeout;
      msgs := Queue.create [];
      pe.state `No_response;
      let req = Devinfo Get_fpga_ver in
      send_msg sender req |> ignore;
      let timer = Timer.create ~step_duration timeout in
      `Continue (step_detect_fpga_ver timer req None)

    and on_timeout t _ =
      Logs.warn (fun m ->
          let s = fmt "timeout (%d sec) reached while detect or init step, restarting..." (Timer.period t) in
          m "%s" s);
      first_step ()

    and step_detect_fpga_ver (timer:Timer.t) req acc recvd =
      find_resp timer req acc recvd
        ~on_success:(fun x _ ->
          Logs.debug (fun m -> m "%s" (fmt "detect step - got fpga version: %d" x));
          let r = Devinfo Get_hw_ver in
          send r; `Continue (step_detect_hw_ver (Timer.reset timer) r x None))
        ~on_failure:(fun timer acc -> `Continue (step_detect_fpga_ver timer req acc))
        ~on_timeout

    and step_detect_hw_ver (timer:Timer.t) req conf acc recvd =
      find_resp timer req acc recvd
        ~on_success:(fun x _ ->
          Logs.debug (fun m -> m "%s" (fmt "detect step - got hw version: %d" x));
          let r = Devinfo Get_fw_ver in
          send r; `Continue (step_detect_fw_ver (Timer.reset timer) r (conf,x) None))
        ~on_failure:(fun timer acc -> `Continue (step_detect_hw_ver timer req conf acc))
        ~on_timeout

    and step_detect_fw_ver (timer:Timer.t) req ((fpga,hw) as conf) acc recvd =
      find_resp timer req acc recvd
        ~on_success:(fun x _ ->
          Logs.debug (fun m -> m "%s" (fmt "detect step - got fw version: %d" x));
          let r = Devinfo Get_serial in
          send r; `Continue (step_detect_serial (Timer.reset timer) r (fpga,hw,x) None))
        ~on_failure:(fun timer acc -> `Continue (step_detect_fw_ver timer req conf acc))
        ~on_timeout

    and step_detect_serial (timer:Timer.t) req ((fpga,hw,fw) as conf) acc recvd =
      find_resp timer req acc recvd
        ~on_success:(fun x _ ->
          Logs.debug (fun m -> m "%s" (fmt "detect step - got serial: %d" x));
          let r = Devinfo Get_type in
          send r; `Continue (step_detect_type (Timer.reset timer) r (fpga,hw,fw,x) None))
        ~on_failure:(fun timer acc -> `Continue (step_detect_serial timer req conf acc))
        ~on_timeout

    and step_detect_type (timer:Timer.t) req ((fpga,hw,fw,ser) as conf) acc recvd =
      find_resp timer req acc recvd
        ~on_success:(fun x _ ->
          Logs.debug (fun m -> m "%s" (fmt "detect step - got type: %d" x));
          let r = Nw Get_mac in
          send r; `Continue (step_detect_mac (Timer.reset timer) r (fpga,hw,fw,ser,x) None))
        ~on_failure:(fun timer acc -> `Continue (step_detect_type timer req conf acc))
        ~on_timeout

    and step_detect_mac (timer:Timer.t) req conf acc recvd =
      find_resp timer req acc recvd
        ~on_success:(fun mac _ ->
          Logs.debug (fun m -> m "%s" (fmt "detect step - got mac: %s" @@ Macaddr.to_string mac));
          let fpga_ver,hw_ver,fw_ver,serial,typ = conf in
          let r = Overall (Set_mode Ip2asi) in
          pe.devinfo (Some { fpga_ver; hw_ver; fw_ver; serial; typ; mac });
          pe.state `Init;
          Logs.info (fun m -> m "%s" (fmt "connection established, board initialization started..."));
          send r; `Continue (step_init_mode (Timer.reset timer) r None))
        ~on_failure:(fun timer acc -> `Continue (step_detect_mac timer req conf acc))
        ~on_timeout

    and step_detect_after_init ns (timer:Timer.t) steps req acc recvd =
      find_resp timer req acc recvd
        ~on_success:(fun _ _ -> `Continue (step_wait_after_detect ns (Timer.create ~step_duration timeout)))
        ~on_failure:(fun timer acc -> `Continue (step_detect_after_init ns timer steps req acc))
        ~on_timeout:(fun timer acc ->
          if steps > 0
          then (send_msg sender req |> Lwt.ignore_result;
                `Continue (step_detect_after_init ns (Timer.reset timer) (pred steps) req acc))
          else on_timeout timer acc)

    and step_wait_after_detect ns (timer:Timer.t) _ =
      try
        `Continue (step_wait_after_detect ns (Timer.step timer))
      with
      | Timer.Timeout _ ->
         let timer = Timer.create ~step_duration timeout in
         match ns with
         | `Mode -> let r = Overall (Set_application Normal) in
                    send r; `Continue (step_init_application timer r None)
         | `App  -> let r = Overall (Set_storage Flash) in
                    send r; `Continue (step_init_storage timer r None)
         | `Nw   -> let r = Ip (Set_enable storage#get.ip.enable) in
                    send r; `Continue (step_init_ip_enable timer r None)

    and step_init_mode (timer:Timer.t) req acc recvd =
      find_resp timer req acc recvd
        ~on_success:(fun x _ ->
          Logs.debug (fun m -> m "%s" (fmt "init step - mode set: %s" @@ mode_to_string x));
          let r = Devinfo Get_fpga_ver in
          send r; `Continue (step_detect_after_init `Mode (Timer.reset timer) reboot_steps r None))
        ~on_failure:(fun timer acc -> `Continue (step_init_mode timer req acc))
        ~on_timeout

    and step_init_application (timer:Timer.t) req acc recvd =
      find_resp timer req acc recvd
        ~on_success:(fun x _ ->
          Logs.debug (fun m ->
              let s = (fmt "init step - application set: %s" @@ application_to_string x) in
              m "%s" s);
          let r = Devinfo Get_fpga_ver in
          send r; `Continue (step_detect_after_init `App (Timer.reset timer) reboot_steps r None))
        ~on_failure:(fun timer acc -> `Continue (step_init_application timer req acc))
        ~on_timeout

    and step_init_storage (timer:Timer.t) req acc recvd =
      find_resp timer req acc recvd
        ~on_success:(fun x _ ->
          Logs.debug (fun m ->
              let s = (fmt "init step - storage set: %s" @@ storage_to_string x) in
              m "%s" s);
          let r = Nw Get_ip in
          send r; `Continue (step_get_ip (Timer.reset timer) r None))
        ~on_failure:(fun timer acc -> `Continue (step_init_storage timer req acc))
        ~on_timeout

    and step_get_ip (timer:Timer.t) req acc recvd =
      find_resp timer req acc recvd
        ~on_success:(fun resp _ ->
          Logs.debug (fun m ->
              let s = (fmt "init step - got ip: %s" @@ Ipaddr.V4.to_string resp) in
              m "%s" s);
          if Ipaddr.V4.equal resp storage#get.nw.ip
          then (let r = Nw Get_mask in
                Logs.debug (fun m -> m "%s" (fmt "init step - no need to set ip, skipping..."));
                send r; `Continue (step_get_mask false (Timer.reset timer) r None))
          else (let r = Nw (Set_ip storage#get.nw.ip) in
                send r; `Continue (step_init_ip (Timer.reset timer) r None)))
        ~on_failure:(fun timer acc -> `Continue (step_get_ip timer req acc))
        ~on_timeout

    and step_init_ip (timer:Timer.t) req acc recvd =
      find_resp timer req acc recvd
        ~on_success:(fun x _ ->
          Logs.debug (fun m ->
              let s = (fmt "init step - ip set: %s" @@ Ipaddr.V4.to_string x) in
              m "%s" s);
          let r = Nw Get_mask in
          send r; `Continue (step_get_mask true (Timer.reset timer) r None))
        ~on_failure:(fun timer acc -> `Continue (step_init_ip timer req acc))
        ~on_timeout

    and step_get_mask need_reboot (timer:Timer.t) req acc recvd =
      find_resp timer req acc recvd
        ~on_success:(fun resp _ ->
          Logs.debug (fun m ->
              let s = (fmt "init step - got mask: %s" @@ Ipaddr.V4.to_string resp) in
              m "%s" s);
          if Ipaddr.V4.equal resp storage#get.nw.mask
          then (let r = Nw Get_gateway in
                Logs.debug (fun m -> m "%s" (fmt "init step - no need to set mask, skipping..."));
                send r; `Continue (step_get_gateway need_reboot (Timer.reset timer) r None))
          else (let r = Nw (Set_mask storage#get.nw.mask) in
                send r; `Continue (step_init_mask (Timer.reset timer) r None)))
        ~on_failure:(fun timer acc -> `Continue (step_get_mask need_reboot timer req acc))
        ~on_timeout

    and step_init_mask (timer:Timer.t) req acc recvd =
      find_resp timer req acc recvd
        ~on_success:(fun x _ ->
          Logs.debug (fun m ->
              let s = (fmt "init step - mask set: %s" @@ Ipaddr.V4.to_string x) in
              m "%s" s);
          let r = Nw Get_gateway in
          send r; `Continue (step_get_gateway true (Timer.reset timer) r None))
        ~on_failure:(fun timer acc -> `Continue (step_init_mask timer req acc))
        ~on_timeout

    and step_get_gateway need_reboot (timer:Timer.t) req acc recvd =
      find_resp timer req acc recvd
        ~on_success:(fun resp _ ->
          Logs.debug (fun m ->
              let s = (fmt "init step - got gateway: %s" @@ Ipaddr.V4.to_string resp) in
              m "%s" s);
          if Ipaddr.V4.equal resp storage#get.nw.gateway
          then (let r = Nw Get_dhcp in
                Logs.debug (fun m -> m "%s" (fmt "init step - no need to set gateway, skipping..."));
                send r; `Continue (step_get_dhcp need_reboot (Timer.reset timer) r None))
          else (let r = Nw (Set_gateway storage#get.nw.gateway) in
                send r; `Continue (step_init_gateway (Timer.reset timer) r None)))
        ~on_failure:(fun timer acc -> `Continue (step_get_gateway need_reboot timer req acc))
        ~on_timeout

    and step_init_gateway (timer:Timer.t) req acc recvd =
      find_resp timer req acc recvd
        ~on_success:(fun x _ ->
          Logs.debug (fun m ->
              let s = (fmt "init step - gateway set: %s" @@ Ipaddr.V4.to_string x) in
              m "%s" s);
          let r = Nw Get_dhcp in
          send r; `Continue (step_get_dhcp true (Timer.reset timer) r None))
        ~on_failure:(fun timer acc -> `Continue (step_init_gateway timer req acc))
        ~on_timeout

    and step_get_dhcp need_reboot (timer:Timer.t) req acc recvd =
      find_resp timer req acc recvd
        ~on_success:(fun resp _ ->
          Logs.debug (fun m ->
              let s = (fmt "init step - got DHCP: %s" @@ string_of_bool resp) in
              m "%s" s);
          let timer = Timer.reset timer in
          if Equal.bool resp storage#get.nw.dhcp
          then
            (Logs.debug (fun m -> m "%s" (fmt "init step - no need to set DHCP, skipping..."));
             step_need_reboot timer need_reboot)
          else (let r = Nw (Set_dhcp storage#get.nw.dhcp) in
                send r; `Continue (step_init_dhcp timer r None)))
        ~on_failure:(fun timer acc -> `Continue (step_get_dhcp need_reboot timer req acc))
        ~on_timeout


    and step_init_dhcp (timer:Timer.t) req acc recvd =
      find_resp timer req acc recvd
        ~on_success:(fun x _ ->
          Logs.debug (fun m ->
              let s = (fmt "init step - DHCP set: %s" @@ string_of_bool x) in
              m "%s" s);
          step_need_reboot (Timer.reset timer) true)
        ~on_failure:(fun timer acc -> `Continue (step_init_dhcp timer req acc))
        ~on_timeout

    and step_need_reboot (timer:Timer.t) = function
      | true  -> Logs.debug (fun m -> m "%s" (fmt "init step - rebooting..."));
                 let r = (Nw Reboot) in
                 send r; `Continue (step_finalize_nw_init timer r None)
      | false -> let r = Ip (Set_enable storage#get.ip.enable) in
                 send r; `Continue (step_init_ip_enable timer r None)

    and step_finalize_nw_init (timer:Timer.t) req acc recvd =
      find_resp timer req acc recvd
        ~on_success:(fun _ _ ->
          let r = Devinfo Get_fpga_ver in
          send r; `Continue (step_detect_after_init `Nw (Timer.reset timer) reboot_steps r None))
        ~on_failure:(fun timer acc -> `Continue (step_finalize_nw_init timer req acc))
        ~on_timeout

    and step_init_ip_enable (timer:Timer.t) req acc recvd =
      find_resp timer req acc recvd
        ~on_success:(fun x _ ->
          Logs.debug (fun m ->
              let s = (fmt "init step - ip enable set: %s" @@ string_of_bool x) in
              m "%s" s);
          let r = Ip (Set_fec_enable storage#get.ip.fec) in
          send r; `Continue (step_init_ip_fec (Timer.reset timer) r None))
        ~on_failure:(fun timer acc -> `Continue (step_init_ip_enable timer req acc))
        ~on_timeout

    and step_init_ip_fec (timer:Timer.t) req acc recvd =
      find_resp timer req acc recvd
        ~on_success:(fun x _ ->
          Logs.debug (fun m ->
              let s = (fmt "init step - fec enable set: %s" @@ string_of_bool x) in
              m "%s" s);
          let r = Ip (Set_udp_port storage#get.ip.port) in
          send r; `Continue (step_init_ip_udp_port (Timer.reset timer) r None))
        ~on_failure:(fun timer acc -> `Continue (step_init_ip_fec timer req acc))
        ~on_timeout

    and step_init_ip_udp_port (timer:Timer.t) req acc recvd =
      find_resp timer req acc recvd
        ~on_success:(fun x _ ->
          Logs.debug (fun m ->
              let s = (fmt "init step - udp port set: %s" @@ string_of_int x) in
              m "%s" s);
          (match storage#get.ip.multicast with
           | Some x -> let r = Ip (Set_mcast_addr x) in
                       send r; `Continue (step_init_ip_multicast (Timer.reset timer) r None)
           | None   -> let r = Ip (Set_method Unicast) in
                       send r; `Continue (step_init_ip_method timer r None)))
        ~on_failure:(fun timer acc -> `Continue (step_init_ip_udp_port timer req acc))
        ~on_timeout

    and step_init_ip_multicast (timer:Timer.t) req acc recvd =
      find_resp timer req acc recvd
        ~on_success:(fun x _ ->
          Logs.debug (fun m ->
              let s = (fmt "init step - multicast address set: %s" @@ Ipaddr.V4.to_string x) in
              m "%s" s);
          let r = Ip (Set_method Multicast) in
          send r; `Continue (step_init_ip_method (Timer.reset timer) r None))
        ~on_failure:(fun timer acc -> `Continue (step_init_ip_multicast timer req acc))
        ~on_timeout

    and step_init_ip_method (timer:Timer.t) req acc recvd =
      find_resp timer req acc recvd
        ~on_success:(fun x _ ->
          Logs.debug (fun m ->
              let s = (fmt "init step - ip method set: %s" @@ meth_to_string x) in
              m "%s" s);
          let r = Ip (Set_delay storage#get.ip.delay) in
          send r; `Continue (step_init_ip_delay (Timer.reset timer) r None))
        ~on_failure:(fun timer acc -> `Continue (step_init_ip_method timer req acc))
        ~on_timeout

    and step_init_ip_delay (timer:Timer.t) req acc recvd =
      find_resp timer req acc recvd
        ~on_success:(fun x _ ->
          Logs.debug (fun m ->
              let s = (fmt "init step - ip delay set: %s" @@ string_of_int x) in
              m "%s" s);
          let r = Ip (Set_rate_est_mode storage#get.ip.rate_mode) in
          send r; `Continue (step_init_ip_rate_mode (Timer.reset timer) r None))
        ~on_failure:(fun timer acc -> `Continue (step_init_ip_delay timer req acc))
        ~on_timeout

    and step_init_ip_rate_mode (timer:Timer.t) req acc recvd =
      find_resp timer req acc recvd
        ~on_success:(fun x _ ->
          Logs.debug (fun m ->
              let s = (fmt "init step - ip rate mode set: %s" @@ rate_mode_to_string x) in
              m "%s" s);
          pe.state `Fine;
          Logs.info (fun m -> m "%s" (fmt "initialization done!"));
          let probes = Probes.make () in
          `Continue (step_ok_probes_send probes None))
        ~on_failure:(fun timer acc -> `Continue (step_init_ip_rate_mode timer req acc))
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
      let responses, acc = Parser.deserialize (concat_acc acc recvd) in
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
    React.E.map (fun (x:status) ->
        let (stream:Common.Stream.stream) =
          { source      = Port 0
          ; id          = `Ts Single
          ; description = Some ""
          }
        in
        if x.asi_bitrate > 0 then List.pure stream else List.empty)
    @@ React.E.changes status
    |> React.S.hold []

  let create sender storage step_duration log_prefix =
    let state,state_push     = React.S.create `No_response in
    let status,status_push   = React.E.create () in
    let config,config_push   = React.E.create () in
    let devinfo,devinfo_push = React.S.create None in

    let s_status = React.S.hold ~eq:(Equal.option equal_status) None
                   @@ React.E.map (fun x -> Some x) status
    in
    let streams  = to_streams_s status in
    let events   = { streams; state; status; config } in
    let (pe:push_events) =
      { state   = state_push
      ; status  = status_push
      ; config  = config_push
      ; devinfo = devinfo_push
      }
    in
    let msgs   = ref (Queue.create []) in
    let send x = send state msgs sender storage pe (Boards.Timer.steps ~step_duration 2) x in
    let fmt fmt = let fs = "%s" ^^ fmt in Printf.sprintf fs log_prefix in
    let log n s = Logs.info (fun m -> m "%s" @@ fmt "got %s set request: %s" n s) in
    let api =
      { set_ip        = (fun x  -> log "ip address" (Ipaddr.V4.to_string x);
                                   send (Nw (Set_ip x)))
      ; set_mask      = (fun x  -> log "network mask" (Ipaddr.V4.to_string x);
                                   send (Nw (Set_mask x)))
      ; set_gateway   = (fun x  -> log "gateway" (Ipaddr.V4.to_string x);
                                   send (Nw (Set_gateway x)))
      ; set_dhcp      = (fun x  -> log "DHCP" (string_of_bool x);
                                   send (Nw (Set_dhcp x)))
      ; set_enable    = (fun x  -> log "enable" (string_of_bool x);
                                   send (Ip (Set_enable x)))
      ; set_fec       = (fun x  -> log "FEC enable" (string_of_bool x);
                                   send (Ip (Set_fec_enable x)))
      ; set_port      = (fun x  -> log "UDP port" (string_of_int x);
                                   send (Ip (Set_udp_port x)))
      ; set_meth      = (fun x  -> log "method" (meth_to_string x);
                                   send (Ip (Set_method x)))
      ; set_multicast = (fun x  -> log "multicast" (Ipaddr.V4.to_string x);
                                   send (Ip (Set_mcast_addr x)))
      ; set_delay     = (fun x  -> log "IP-to-output delay" (string_of_int x);
                                   send (Ip (Set_delay x)))
      ; set_rate_mode = (fun x  -> log "rate estimation mode" (rate_mode_to_string x);
                                   send (Ip (Set_rate_est_mode x)))
      ; reset         = (fun () -> Logs.info (fun m -> m "%s" @@ fmt "got reset request");
                                   send (Nw Reboot))
      ; get_status    = (fun () -> React.S.value s_status)
      ; get_config    = (fun () -> storage#get)
      ; get_devinfo   = (fun () -> React.S.value devinfo)
      }
    in
    events,api,(step msgs sender storage step_duration pe log_prefix)

end
