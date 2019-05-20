open Board_dektec_dtm3200_types

let ( >>= ) = Lwt.( >>= )

let status_interval = 1. (* seconds *)

let cooldown_timeout = 10. (* seconds *)

type api_msg = Cstruct.t Request.cmd Lwt_stream.t -> unit Lwt.t

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
    Lwt_stream.junk_old rsp_queue
    >>= fun () -> Lwt_stream.junk_old req_queue
    >>= detect_device

  and restart () =
    Logs.info (fun m -> m "Restarting...");
    set_state `No_response;
    Lwt_stream.junk_old rsp_queue
    >>= fun () -> Lwt_stream.junk_old req_queue
    >>= fun () -> Lwt_unix.sleep cooldown_timeout
    >>= first_step

  and detect_device () =
    set_state `Detect;
    Fsm_detect.step
      ~return:(fun e -> Logs.err (fun m ->
          m "Error during detect step: %s"
          @@ Request.error_to_string e);
          restart ())
      ~continue:(fun devinfo ->
          set_devinfo devinfo;
          Logs.info (fun m -> m "Connection established, starting initialization...");
          init_device ())
      src sender rsp_queue kv ()

  and init_device () =
    set_state `Init;
    Fsm_init.step
      ~return:(fun e -> Logs.err (fun m ->
          m "Error during init step: %s"
          @@ Request.error_to_string e);
          restart ())
      ~continue:(fun () ->
          set_state `Fine;
          Logs.info (fun m -> m "Initialization done!");
          fork ())
      src sender rsp_queue kv ()

  and pull_status () =
    Fsm_probes.step
      ~return:(fun e -> Logs.err (fun m ->
          m "Error during pulling status: %s"
          @@ Request.error_to_string e);
          restart ())
      ~continue:(fun status ->
          set_status status;
          fork ())
      src sender rsp_queue kv ()

  and fork ?timer () =
    let timer = match timer with
      | Some x -> x
      | None -> Lwt_unix.sleep status_interval >>= fun () -> Lwt.return `S in
    let wait_msg = Lwt_stream.next req_queue >>= fun x -> Lwt.return @@ `M x in
    Lwt.choose [wait_msg; timer]
    >>= function
    | `M send -> send rsp_queue >>= fork ~timer
    | `S -> Lwt.cancel wait_msg; pull_status ()

  in
  first_step
