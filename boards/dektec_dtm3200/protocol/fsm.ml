open Board_dektec_dtm3200_types

let ( >>= ) = Lwt.( >>= )

let status_interval = 1. (* seconds *)

let start ~(address : int)
    (src : Logs.src)
    (sender : Cstruct.t -> unit Lwt.t)
    msgs
    (stream : Cstruct.t Request.cmd Lwt_stream.t)
    (config : config)
    (set_state : Application_types.Topology.state -> unit)
    (set_devinfo : devinfo -> unit)
    (set_status : status -> unit) =

  let (module Logs : Logs.LOG) = Logs.src_log src in

  let rec first_step () =
    Logs.info (fun m -> m "Start of connection establishment...");
    let msgs' = Lwt_stream.get_available msgs in
    List.iter (fun (_, _, stop) -> stop ()) msgs';
    Lwt_stream.junk_old stream
    >>= fun () ->
    set_state `No_response;
    detect_device ()

  and detect_device () =
    Fsm_detect.step ~address
      ~return:(fun () -> Lwt_unix.sleep 5. >>= fun () -> first_step ())
      ~continue:(fun devinfo ->
          set_devinfo devinfo;
          set_state `Init;
          Logs.info (fun m -> m "Connection established, starting initialization...");
          init_device ())
      src sender stream ()

  and init_device () =
    Fsm_init.step ~address
      ~return:(fun () -> Lwt_unix.sleep 5. >>= fun () -> first_step ())
      ~continue:(fun () ->
          set_state `Fine;
          Logs.info (fun m -> m "Initialization done!");
          Lwt_unix.sleep 1. >>= pull_status)
      src sender stream config ()

  and pull_status () =
    Fsm_probes.step ~address
      ~return:(fun () -> Lwt_unix.sleep 5. >>= fun () -> first_step ())
      ~continue:(fun status ->
          set_status status;
          fork ())
      src sender stream ()

  and fork ?timer () =
    let timer = match timer with
      | Some x -> x
      | None ->
        Lwt_unix.sleep status_interval
        >>= fun () -> Lwt.return `Timeout in
    let wait_msg = Lwt_stream.next msgs >>= fun x -> Lwt.return @@ `Message x in
    Lwt.choose [wait_msg; timer]
    >>= function
    | `Message msg -> send_client_request timer msg
    | `Timeout -> Lwt.cancel wait_msg; pull_status ()

  and send_client_request timer (msg, pred, _) =
    sender msg
    >>= fun () -> pred stream
    >>= fork ~timer
  in
  first_step
