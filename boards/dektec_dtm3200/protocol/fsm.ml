open Board_dektec_dtm3200_types

let ( >>= ) = Lwt.( >>= )

let status_interval = 1. (* seconds *)

let cooldown_timeout = 10. (* seconds *)

type api_msg = Cstruct.t Request.cmd Lwt_stream.t -> unit Lwt.t

type 'a set = ?step:React.step -> 'a -> unit

let start
    (src : Logs.src)
    (sender : Cstruct.t -> unit Lwt.t)
    (req_queue : api_msg Lwt_stream.t)
    (rsp_queue : Cstruct.t Request.cmd Lwt_stream.t)
    (kv : config Kv_v.rw)
    (set_state : Application_types.Topology.state set)
    (set_devinfo : devinfo set)
    (set_status : status set) =

  let (module Logs : Logs.LOG) = Logs.src_log src in

  let rec restart () =
    Logs.info (fun m -> m "Restarting...");
    set_state `No_response;
    Lwt_stream.junk_old rsp_queue
    >>= fun () -> Lwt_stream.junk_old req_queue
    >>= fun () -> Lwt_unix.sleep cooldown_timeout
    >>= detect

  and detect () =
    Logs.info (fun m -> m "Start of connection establishment...");
    set_state `Detect;
    Fsm_detect.step
      ~return:(fun e -> Logs.err (fun m ->
          m "Error during detect step: %s"
          @@ Request.error_to_string e);
          restart ())
      ~continue:initialize
      src sender rsp_queue kv ()

  and initialize (devinfo : devinfo) =
    Logs.info (fun m -> m "Connection established, starting initialization...");
    let step = React.Step.create () in
    set_devinfo ~step devinfo;
    set_state ~step `Init;
    React.Step.execute step;
    Fsm_init.step
      ~return:(fun e -> Logs.err (fun m ->
          m "Error during init step: %s"
          @@ Request.error_to_string e);
          restart ())
      ~continue:(fun () ->
          set_state `Fine;
          Logs.info (fun m -> m "Initialization done!");
          idle ())
      src sender rsp_queue kv ()

  and pull_status ~wait_msg () =
    Fsm_probes.step
      ~return:(fun e -> Logs.err (fun m ->
          m "Error during pulling status: %s"
          @@ Request.error_to_string e);
          restart ())
      ~continue:(fun status ->
          set_status status;
          idle ())
      src sender rsp_queue kv ()

  and idle ?wait_msg ?timer () =
    let timer = match timer with
      | Some x -> x
      | None -> Lwt_unix.sleep status_interval >>= fun () -> Lwt.return `S in
    let wait_msg = match wait_msg with
      | Some x -> x
      | None -> Lwt_stream.next req_queue >>= fun x -> Lwt.return @@ `M x in
    Lwt.choose [wait_msg; timer]
    >>= function
    | `M send -> send rsp_queue >>= idle ~timer
    | `S -> pull_status ~wait_msg ()
  in
  detect
