open Board_niitv_tsan_types
open Application_types

let ( >>= ) = Lwt.( >>= )

let start (src : Logs.src)
    (sender : Cstruct.t -> unit Lwt.t)
    (req_queue : api_msg Lwt_stream.t)
    (rsp_queue : Request.msg Lwt_stream.t)
    (kv : config Kv_v.w)
    (sources : init)
    (set_state : Topology.state -> unit) =

  let (module Logs : Logs.LOG) = Logs.src_log src in

  let rec first_step () =
    Logs.info (fun m -> m "Start of connection establishment...");
    Lwt_stream.junk_old req_queue
    >>= fun () -> Lwt_stream.junk_old rsp_queue
    >>= fun () -> set_state `No_response; detect_device ()

  and detect_device () =
    let rec loop () =
      Lwt_stream.next rsp_queue
      >>= function
      | { tag = `Status; _ } -> request src rsp_queue sender Request.Get_devinfo
      | { tag = `Devinfo_rsp; data } -> Lwt.return @@ Parser.parse_devinfo data
      | _ -> loop () in
    loop ()
    >>= function
    | Ok x -> init_device x
    | Error e ->
      Logs.err (fun m ->
          m "Got error during detect step: %s"
          @@ Request.error_to_string e);
      restart ()

  and step_init devinfo (acc : Acc.t) _ =
    push_state pe `Init;
    pe.devinfo (Some devinfo);
    let { t2mi_mode; input; jitter_mode } : config = storage#get in
    let t2mi_mode_raw = t2mi_mode_to_raw t2mi_mode in
    Serializer.send_instant sender (Set_board_init sources)
    |> Lwt.ignore_result;
    Serializer.send_instant sender (Set_board_mode (input, t2mi_mode_raw))
    |> Lwt.ignore_result;
    Serializer.send_instant sender (Set_jitter_mode jitter_mode)
    |> Lwt.ignore_result;
    Logs.info (fun m -> m "connection established, waiting \
                           for 'status' message");
    let exn = Protocol_invariant (Status_timeout status_timeout) in
    let timer = Timer.create ~exn ~step_duration status_timeout in
    `Continue (step_ok_idle timer acc)

  and step_ok_idle (timer : Timer.t) (acc : Acc.t) recvd =
    try
      let _, rsps, acc = deserialize ~with_init_exn:true acc recvd in
      handle_msgs rsps;
      if not (Queue.empty !imsgs)
      then Queue.send !imsgs () |> Lwt.ignore_result;
      imsgs := Queue.next !imsgs;
      begin match Events.partition acc with
        | [], acc -> `Continue (step_ok_idle (Timer.step timer) acc)
        | [group], acc ->
          if Option.is_none acc.group
          then (Logs.info (fun m -> m "initialization done!");
                push_state pe `Fine);
          Logs.debug (fun m ->
              m "received group: load=%g%%" group.status.basic.load);
          let stack = Events.get_req_stack group acc.group in
          let exn = Protocol_invariant (Probes_timeout probes_timeout) in
          let pool =
            List.map (fun req ->
                { send = (fun () -> Serializer.send_event sender req)
                ; pred = Parser.is_probe_response req
                ; timeout = Timer.steps ~step_duration probes_timeout
                ; exn = Some exn }) stack
            |> Pool.create in
          Logs.debug (fun m ->
              let pre = "prepared stack of probe requests" in
              let stk =
                String.concat "; "
                @@ List.map Serializer.probe_req_to_string stack in
              m "%s: [%s]" pre stk);
          let acc = { acc with group = Some group } in
          let acc = Events.handle_immediate pe acc in
          step_ok_probes_send pool (Timer.reset timer) acc
        | _ -> raise_notrace (Protocol_invariant Unexpected_status_idle)
      end
    with Protocol_invariant e -> handle_invariant e

  and step_ok_probes_send pool (timer : Timer.t) (acc : Acc.t) =
    if Pool.empty pool
    then
      let acc = Events.handle pe events sources acc in
      `Continue (step_ok_idle (Timer.reset timer) acc)
    else (Pool.send pool () |> Lwt.ignore_result;
          `Continue (step_ok_probes_wait pool timer acc))

  and step_ok_probes_wait pool (timer : Timer.t) (acc : Acc.t) recvd =
    try
      let probes, rsps, acc = deserialize ~with_init_exn:true acc recvd in
      handle_msgs rsps;
      if not (Queue.empty !imsgs)
      then Queue.send !imsgs () |> Lwt.ignore_result;
      imsgs := Queue.next !imsgs;
      let timer, acc = match Events.partition acc with
        | [], acc -> (Timer.step timer), acc
        | _ -> raise_notrace (Protocol_invariant Unexpected_status_probes) in
      begin match Pool.responsed pool probes with
        | None -> `Continue (step_ok_probes_wait (Pool.step pool) timer acc)
        | Some x ->
          Logs.debug (fun m ->
              m "got probe response: '%s'"
              @@ Serializer.probe_rsp_to_string x);
          let acc = Acc.cons_probe acc x in
          begin match Pool.last pool with
            | true ->
              let (acc : Acc.t) =
                Events.handle pe events sources acc in
              `Continue (step_ok_idle timer acc)
            | false -> step_ok_probes_send (Pool.next pool) timer acc
          end
      end
    with Protocol_invariant e -> handle_invariant e

  in
  first_step ()
