open Board_dektec_dtm3200_types

let ( >>= ) = Lwt.( >>= )

let status_interval = 1. (* seconds *)

type api_msg = (Cstruct.t Request.cmd Lwt_stream.t -> unit Lwt.t)
               * (Request.error -> unit)

let start
    (src : Logs.src)
    (sender : Cstruct.t -> unit Lwt.t)
    (req_queue : api_msg Lwt_stream.t)
    (rsp_queue : Cstruct.t Request.cmd Lwt_stream.t)
    (kv : config Kv_v.rw)
    (set_state : Application_types.Topology.state -> unit)
    (set_devinfo : devinfo -> unit)
    (set_status : status -> unit) =

  let (module Logs : Logs.LOG) = Logs.src_log src in

  let rec first_step () =
    Logs.info (fun m -> m "Start of connection establishment...");
    let msgs' = Lwt_stream.get_available req_queue in
    List.iter (fun (_, stop) -> stop Request.Not_responding) msgs';
    Lwt_stream.junk_old rsp_queue
    >>= fun () ->
    set_state `No_response;
    detect_device ()

  and detect_device () =
    Fsm_detect.step
      ~return:(fun () -> Lwt_unix.sleep 5. >>= fun () -> first_step ())
      ~continue:(fun devinfo ->
          set_devinfo devinfo;
          set_state `Init;
          Logs.info (fun m -> m "Connection established, starting initialization...");
          init_device ())
      src sender rsp_queue kv ()

  and init_device () =
    Fsm_init.step
      ~return:(fun () -> Lwt_unix.sleep 5. >>= fun () -> first_step ())
      ~continue:(fun () ->
          set_state `Fine;
          Logs.info (fun m -> m "Initialization done!");
          Lwt_unix.sleep 1. >>= pull_status)
      src sender rsp_queue kv ()

  and pull_status () =
    Fsm_probes.step
      ~return:(fun () -> Lwt_unix.sleep 5. >>= fun () -> first_step ())
      ~continue:(fun status ->
          set_status status;
          fork ())
      src sender rsp_queue kv ()

  and fork ?timer () =
    let timer = match timer with
      | Some x -> x
      | None ->
        Lwt_unix.sleep status_interval
        >>= fun () -> Lwt.return `Timeout in
    let wait_msg = Lwt_stream.next req_queue >>= fun x -> Lwt.return @@ `Message x in
    Lwt.choose [wait_msg; timer]
    >>= function
    | `Message msg -> send_client_request timer msg
    | `Timeout -> Lwt.cancel wait_msg; pull_status ()

  and send_client_request timer (send, _) =
    send rsp_queue >>= fork ~timer
  in
  first_step
