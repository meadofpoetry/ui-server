open Application_types
open Boards
open Board_dektec_dtm3200_types
open Util_react
open Netlib

let ( >>= ) = Lwt.bind

type notifs =
  { streams : Stream.Raw.t list React.signal
  ; state : Topology.state React.signal
  ; status : status React.event
  ; config : config React.event
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

let step ~(address : int)
      (src : Logs.src)
      msgs
      (sender : Cstruct.t -> unit Lwt.t)
      (config : config React.signal)
      (pe : Sm_common.push_events) =

  let rec first_step () =
    Logs.info ~src (fun m -> m "start of connection establishment...");
    msgs := Pools.Queue.invalidate !msgs;
    pe.state `No_response;
    Sm_detect.step ~address
      ~entry_point:first_step
      ~exit_point:(fun () ->
        Sm_init.step ~address
          ~entry_point:first_step
          ~exit_point:(fun () -> fork None)
          src sender pe ())
      src sender pe ()

  and fork acc =
    match Pools.Queue.is_empty !msgs with
    | true ->
       Sm_probes.step
         ~address
         ~entry_point:first_step
         ~exit_point:(fun () -> fork None)
         src sender pe ()
    | false -> send_client_request acc

  and send_client_request acc =
    if Pools.Queue.is_empty !msgs
    then fork acc
    else (Pools.Queue.send !msgs
          >>= fun msgs' ->
          msgs := msgs';
          Lwt.return @@ `Continue (wait_client_request acc))

  and wait_client_request acc recvd =
    let responses, acc =
      Parser.deserialize ~address src (Board.concat_acc acc recvd) in
    Pools.Queue._match !msgs
      ~pending:(fun pool ->
        msgs := pool;
        Lwt.return @@ `Continue (wait_client_request acc))
      ~resolved:(fun pool _ ->
        msgs := pool;
        send_client_request acc)
      ~not_sent:(fun pool ->
        msgs := pool;
        send_client_request acc)
      ~error:(fun pool -> function
        | `Interrupted ->
           msgs := pool;
           send_client_request acc
        | `Timeout ->
           Logs.warn ~src (fun m ->
               m "timeout while waiting for client request response, restarting...");
           first_step ())

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
