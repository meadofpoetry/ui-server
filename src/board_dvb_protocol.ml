open Common.Board.Dvb
open Lwt.Infix
open Board_meta

include Board_dvb_parser

let ( % ) = CCFun.(%)
      
(* Board protocol implementation *)

let timeout_period step_duration = 2 * int_of_float (1. /. step_duration) (* 2 secs *)
          
let request_period step_duration = 2 * int_of_float (1. /. step_duration) (* 5 secs *)

let detect = Devinfo

let init = [ Settings (0, { mode = T2
                          ; bw   = Bw8
                          ; freq = 586000000l
                          ; plp  = 0})
           ; Settings (1, { mode = T2
                          ; bw   = Bw8
                          ; freq = 586000000l
                          ; plp  = 0})
           ; Settings (2, { mode = T2
                          ; bw   = Bw8
                          ; freq = 586000000l
                          ; plp  = 0})
           ; Settings (3, { mode = T2
                          ; bw   = Bw8
                          ; freq = 586000000l
                          ; plp  = 0})]

let detect_msgs (send_req : 'a request -> unit Lwt.t) =
  [ { send = (fun () -> send_req detect); pred = (is_response detect) } ]

let init_msgs (send_req : 'a request -> unit Lwt.t) =
  List.map (fun x ->
      { send = (fun () -> send_req x)
      ; pred = (is_response x)
    })
    init
                            
let measure_probes (send_ev : 'a event_request -> unit Lwt.t) config =
  List.map (fun x ->
      { send = (fun () -> send_ev @@ Measure x)
      ; pred = (is_event (Measure x))
    })
    config.modules               
  
module SM = struct

  type event = [ `Measure of (int * Cbuffer.t) ]

  let wakeup_timeout t = t.pred `Timeout |> ignore

  type push_events = { measure : (int * rsp_measure) -> unit
                     }

  let event_push pe = function
    | `Measure (id, buf) -> pe.measure (id, of_rsp_measure_exn buf)

  let send_msg (type a) sender (msg : a request) : unit Lwt.t =
    match msg with
    | Devinfo    -> sender @@ (to_req_devinfo false)
    | Reset      -> sender @@ (to_req_devinfo true)
    | Settings (id, buf) -> sender @@ (to_req_settings id buf)
    | Plp_setting (id, buf) -> sender @@ (to_req_plp_set id buf)
    | Plps id  -> sender @@ (to_req_plp_list id)

  let send_event (type a) sender (msg : a event_request) : unit Lwt.t =
    (* no instant msgs *)
    match msg with
    | Measure id -> sender @@ to_req_measure id

  let send (type a) msgs sender (msg : a request) : a Lwt.t =
    (* no instant msgs *)
    let t, w = Lwt.wait () in
    let pred = function
      | `Timeout -> Lwt.wakeup_exn w (Failure "msg timeout"); None
      | l -> CCOpt.( is_response msg l >|= Lwt.wakeup w ) in
    let send = fun () -> send_msg sender msg in
    msgs := Msg_queue.append !msgs { send; pred };
    t

  let initial_timeout = -1
                      
  let step msgs sender step_duration push_state push_events =
    let period         = timeout_period step_duration in
    let request_period = request_period step_duration in
    let push_events  = event_push push_events in
    let detect_pool  = Msg_pool.create period (detect_msgs (send_msg sender)) in

    let rec first_step () =
      Msg_queue.iter !msgs wakeup_timeout;
      msgs := Msg_queue.create period [];
      push_state `No_response;
      Msg_pool.send detect_pool () |> ignore;
      `Continue (step_detect detect_pool None)
    
    and step_detect detect_pool acc recvd =
      try
        Lwt_io.printf "Detect step\n" |> ignore;
        let recvd = Board_meta.concat_acc acc recvd in
        let _, responses, acc = deserialize recvd in
        match Msg_pool.responsed detect_pool responses with
        | Some detect -> step_start_init (measure_probes (send_event sender) detect)
        | _           -> if detect_pool.timer > 0
                         then `Continue (step_detect (Msg_pool.step detect_pool) acc)
                         else let detect_pool = Msg_pool.next detect_pool in
                              Msg_pool.send detect_pool () |> ignore;
                              `Continue (step_detect detect_pool acc)
      with Timeout -> first_step ()

    and step_start_init probes =
      try
        match init_msgs (send_msg sender) with
        | [] -> let probes_pool = Msg_pool.create period probes in
                Msg_pool.send probes_pool () |> ignore;
                `Continue (step_normal_probes probes_pool 0 None)
        | lst -> let init_pool = Msg_pool.create period lst in
                 Msg_pool.send init_pool () |> ignore;
                 `Continue (step_init init_pool probes None)
      with Timeout -> first_step ()
                 
    and step_init init_pool probes acc recvd =
      try
        Lwt_io.printf "Init step\n" |> ignore;
        let recvd = Board_meta.concat_acc acc recvd in
        let _, responses, acc = deserialize recvd in
        match Msg_pool.responsed init_pool responses with
        | None    -> if init_pool.timer < 0
                     then (first_step ())
                     else `Continue (step_init (Msg_pool.step init_pool) probes acc)
        | Some _  ->
           (match Msg_pool.last init_pool with
            | true   -> push_state `Fine;
                        let probes_pool = Msg_pool.create period probes in
                        `Continue (step_normal_start probes_pool 0 acc)
            | false  -> let init_pool = Msg_pool.next init_pool in
                        Msg_pool.send init_pool () |> ignore;
                        `Continue (step_init init_pool probes acc))
      with Timeout -> first_step ()

    and step_normal_start probes_pool period_timer acc _ =
      Lwt_io.printf "Normal step start\n" |> ignore;
      if not @@ Msg_pool.empty probes_pool
      then Msg_pool.send probes_pool () |> ignore;
      `Continue (step_normal_probes probes_pool (succ period_timer) acc)
                             
    and step_normal_probes probes_pool period_timer acc recvd =
      (*Lwt_io.printf "Normal step probes\n" |> ignore;*)
      let recvd = Board_meta.concat_acc acc recvd in
      let events, _, acc = deserialize recvd in

      try
        let sent, probes_pool = if Msg_pool.empty probes_pool
                                then false, probes_pool
                                else (match Msg_pool.responsed probes_pool events with
                                      | None    -> false, Msg_pool.step probes_pool
                                      | Some () -> let probes_pool = Msg_pool.next probes_pool in
                                                   Msg_pool.send probes_pool () |> ignore;
                                                   true, probes_pool)
        in
        CCList.iter push_events events;
        if Msg_pool.last probes_pool
        then `Continue (step_normal_probes_cleanup sent probes_pool (succ period_timer) acc)
        else `Continue (step_normal_probes probes_pool (succ period_timer) acc)
      with Timeout -> first_step ()

    and step_normal_probes_cleanup sent probes_pool period_timer acc recvd =
      Lwt_io.printf "Normal step probes cleanup %f\n" ((float_of_int period_timer) *. step_duration) |> ignore;

      if not sent
      then (step_normal_requests (Msg_pool.next probes_pool) period_timer acc recvd)
      else
        let recvd = Board_meta.concat_acc acc recvd in
        let events, _, acc = deserialize recvd in
        try
          let recvd = if Msg_pool.empty probes_pool
                      then true
                      else (match Msg_pool.responsed probes_pool events with
                            | None    -> false
                            | Some () -> true)
          in
          CCList.iter push_events events;
          if recvd
          then let probes_pool = (Msg_pool.next probes_pool) in
               if (period_timer > request_period)
               then raise (Failure "board_dvb sm invariant broken")
               else `Continue (step_normal_requests probes_pool (succ period_timer) acc)
          else `Continue (step_normal_probes_cleanup sent probes_pool (succ period_timer) acc)
        with Timeout -> first_step () 

    and step_normal_requests probes_pool period_timer acc recvd =
      (*Lwt_io.printf "Normal step requests\n" |> ignore;*)
      let recvd = Board_meta.concat_acc acc recvd in
      let _, responses, acc = deserialize recvd in

      try
        let sent =
          if not @@ Msg_queue.empty !msgs
          then begin match Msg_queue.responsed !msgs responses with
               | None    -> msgs := Msg_queue.step !msgs; false
               | Some () -> msgs := Msg_queue.next !msgs;
                            Msg_queue.send !msgs () |> ignore;
                            true
               end
          else false
        in
        if (period_timer >= request_period)
        then `Continue (step_normal_requests_cleanup sent probes_pool (succ period_timer) acc)
        else `Continue (step_normal_requests probes_pool (succ period_timer) acc)
      with Timeout -> first_step ()

    and step_normal_requests_cleanup sent probes_pool period_timer acc recvd =
      Lwt_io.printf "Normal step requests cleanup\n" |> ignore;

      if not sent
      then step_normal_start probes_pool ((succ period_timer) mod request_period) acc recvd
      else
        let recvd = Board_meta.concat_acc acc recvd in
        let _, responses, acc = deserialize recvd in
        try
          let recvd =
            match Msg_queue.responsed !msgs responses with
            | None    -> msgs := Msg_queue.step !msgs; false
            | Some () -> msgs := Msg_queue.next !msgs; true
          in
          if recvd
          then `Continue (step_normal_start probes_pool ((succ period_timer) mod request_period) acc)
          else `Continue (step_normal_requests_cleanup sent probes_pool (succ period_timer) acc)
        with Timeout -> first_step ()
                      
    in
    first_step ()
    
  let create sender push_state step_duration =
    let period = 5 * (int_of_float (1. /. step_duration)) in
    let measure, mpush = React.E.create () in
    let (events : events) = { measure } in
    let push_events = { measure = mpush } in
    let msgs = ref (Msg_queue.create period []) in
    events,
    (send msgs sender),
    (step msgs sender step_duration push_state push_events)

end
