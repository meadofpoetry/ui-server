open Application_types
open Boards
open Board_dektec_dtm3200_types
open Util_react
open Netlib

let ( >>= ) = Lwt.bind

type notifs =
  { streams : Stream.t list React.signal
  ; state : Topology.state React.signal
  ; status : status React.event
  ; config : config React.signal
  }

type api =
  { address : int
  ; kv : config Kv_v.rw
  ; notifs : notifs
  ; channel : 'a. 'a Request.t -> ('a, error) Lwt_result.t
  ; loop : (Cstruct.t option -> 'c Board.cc Lwt.t as 'c) Board.cc
  }
and error =
  | Not_responding
  | Timeout

let step ~(address : int)
      (src : Logs.src)
      msgs
      (sender : Cstruct.t -> unit Lwt.t)
      (pe : Sm_common.push_events) =

  let rec first_step () =
    Logs.info ~src (fun m -> m "start of connection establishment...");
    msgs := Pools.Queue.invalidate !msgs;
    pe.state `No_response;
    detect_device ()

  and detect_device () =
    Sm_detect.step ~address
      ~return:(fun () -> Lwt_unix.sleep 5. >>= fun () -> first_step ())
      ~continue:init_device
      src sender pe ()

  and init_device () =
    Sm_init.step ~address
      ~return:(fun () -> Lwt_unix.sleep 5. >>= fun () -> first_step ())
      ~continue:pull_status
      src sender pe ()

  and pull_status () =
    Sm_probes.step ~address
      ~return:(fun () -> Lwt_unix.sleep 5. >>= fun () -> first_step ())
      ~continue:fork
      src sender pe ()

  and fork ?timer () =
    let timer = match timer with
      | Some x -> x
      | None -> Lwt_unix.sleep Sm_probes.interval >>= pull_status in
    Lwt.pick
      [ timer
      ; send_client_request timer ()
      ]

  and send_client_request timer () =
    if Pools.Queue.is_empty !msgs
    then fork ~timer ()
    else (
      Pools.Queue.send !msgs
      >>= fun msgs' ->
      msgs := msgs';
      Lwt.return @@ `Continue (wait_client_request timer None))

  and wait_client_request timer acc recvd =
    let responses, acc = match Board.concat_acc acc recvd with
      | None -> [], None
      | Some recvd -> Parser.deserialize ~address src recvd in
    Pools.Queue._match !msgs
      ~pending:(fun pool ->
        msgs := pool;
        Lwt.return @@ `Continue (wait_client_request timer acc))
      ~resolved:(fun pool _ -> msgs := pool; fork ~timer ())
      ~not_sent:(fun pool -> msgs := pool; send_client_request timer ())
      ~error:(fun pool -> function
        | `Interrupted -> msgs := pool; fork ~timer ()
        | `Timeout ->
           Logs.warn ~src (fun m ->
               m "timeout while waiting for client request response, restarting...");
           Lwt.cancel timer;
           first_step ())
  in
  `Continue (fun _ -> first_step ())

let to_streams_s (config : config signal) (status : status event) =
  S.hold ~eq:(Util.List.equal Stream.Raw.equal) []
  @@ S.sample (fun ({ asi_bitrate; protocol; _ } : status)
                   ({ ip; nw } : config) ->
         if asi_bitrate <= 0 then [] else (
           let scheme = match protocol with
             | RTP -> "rtp"
             | UDP -> "udp" in
           let (info : Stream.Source.ipv4) = match ip.multicast with
             | Some x ->
                { addr = x
                ; port = ip.port
                ; scheme
                }
             | None ->
                { addr = nw.ip
                ; port = ip.port
                ; scheme
                } in
           let (stream : Stream.Raw.t) =
             { source = { info = IPV4 info; node = Port 0 }
             ; id = TS_raw
             ; typ = TS
             } in
           [stream]))
       status config

let create ~(address : int)
      (src : Logs.src)
      (sender : Cstruct.t -> unit Lwt.t)
      streams_conv
      (kv : config Kv_v.rw)
      (control : int)
      (db : Db.t) =
  let state, set_state = S.create ~eq:Topology.equal_state `No_response in
  let status, set_status = E.create () in
  let devinfo, set_devinfo = S.create ~eq:(Util.Option.equal equal_devinfo) None in
  let notifs =
    { streams = streams_conv @@ to_streams_s kv#s status
    ; state
    ; status
    ; config = kv#s
    } in
  let (pe : Sm_common.push_events) =
    { state = set_state
    ; status = set_status
    ; devinfo = set_devinfo
    } in
  let msgs = ref (Pools.Queue.create []) in
  let loop = step ~address src msgs sender pe in
  let (api : api) =
    { notifs
    ; address
    ; loop
    ; kv
    ; channel = (fun _ -> assert false)
    } in
  Lwt.return_ok api
