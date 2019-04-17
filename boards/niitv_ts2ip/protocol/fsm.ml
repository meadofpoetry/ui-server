let start (src : Logs.src)
    (sender : Cstruct.t -> unit Lwt.t) =

  let (module Logs : Logs.LOG) = Logs.src_log src in

  let wakeup_timeout (_, t) = t.pred `Timeout |> ignore in
  let events_push _ = function
    | `Status (brd,General x) ->
      let sms = storage#get.packers in
      let pkrs = List.take (List.length sms) x in
      let status = { board_status   = brd
                   ; packers_status = List.map2 Pair.make sms pkrs
                   } in
      Logs.debug (fun m -> m "got status event: %s" @@ show_status status);
      pe.status status
    | _ -> () in

  let rec first_step () =
    Logs.info (fun m -> m "Start of connection establishment...");
    Await_queue.iter !msgs wakeup_timeout;
    msgs  := Await_queue.create [];
    imsgs := Queue.create [];
    pe.state `No_response;
    send_msg sender Get_board_info |> Lwt.ignore_result;
    `Continue (step_detect (Timer.create ~step_duration timeout) None)

  and step_detect (timer : Timer.t) acc recvd =
    try
      let _, rsps, acc = Parser.deserialize (concat_acc acc recvd) in
      match List.find_map (is_response Get_board_info) rsps with
      | Some r ->
        pe.state `Init;
        pe.devinfo (Some r);
        let config = storage#get in
        send_instant sender (Set_factory_mode config.factory_mode)
        |> Lwt.ignore_result;
        send_instant sender (Set_board_mode (config.nw_mode,config.packers))
        |> Lwt.ignore_result;
        Logs.info (fun m -> m "connection established, \
                               waiting for 'status' message");
        `Continue (step_ok_idle true r (Timer.reset timer) None)
      | None -> `Continue (step_detect (Timer.step timer) acc)
    with Timer.Timeout t ->
      Logs.warn (fun m ->
          m "connection is not established after %g seconds, \
             restarting..." (Timer.period t));
      first_step ()

  and step_ok_idle is_init info (timer : Timer.t) acc recvd =
    try
      let events, rsps, acc = Parser.deserialize (concat_acc acc recvd) in
      if has_board_info rsps then raise_notrace Unexpected_init;
      Queue.send !imsgs () |> Lwt.ignore_result;
      imsgs := Queue.next !imsgs;
      match events with
      | [] -> `Continue (step_ok_idle is_init info (Timer.step timer) acc)
      | l  ->
        if is_init
        then (Logs.info (fun m -> m "initialization done!");
              pe.state `Fine);
        List.iter (events_push info) l;
        `Continue (step_ok_idle false info (Timer.reset timer) acc)
    with
    | Unexpected_init ->
      Logs.warn (fun m -> m "%s" board_info_err_msg);
      first_step ()
    | Timer.Timeout t ->
      Logs.warn (fun m -> m "%s" @@ no_status_msg t);
      first_step ()

  in first_step ()
