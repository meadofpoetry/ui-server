open Common.Board.Dvb
open Lwt.Infix
open Board_meta

include Board_dvb_parser

let ( % ) = CCFun.(%)
      
(* Board protocol implementation *)

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

let plp_probes (send_ev : 'a event_request -> unit Lwt.t) config =
  List.map (fun x ->
      { send = (fun () -> send_ev @@ Plps x)
      ; pred = (is_event (Plps x))
    })
    config.modules                 
  
module SM = struct

  type event = [ `Measure of (int * Cbuffer.t)
               | `Plps    of (int * Cbuffer.t)
               ]

  let wakeup_timeout t = t.pred `Timeout |> ignore

  type push_events = { measure : (int * rsp_measure) -> unit
                     ; plps    : (int * rsp_plp_list) -> unit
                     }

  let event_push pe = function
    | `Measure (id, buf) -> pe.measure (id, of_rsp_measure_exn buf)
    | `Plps    (id, buf) -> pe.plps (id, of_rsp_plp_list_exn buf)

  let send_msg (type a) sender (msg : a request) : unit Lwt.t =
    match msg with
    | Devinfo    -> sender @@ (to_req_devinfo false)
    | Reset      -> sender @@ (to_req_devinfo true)
    | Settings (id, buf) -> sender @@ (to_req_settings id buf)
    | Plp_setting (id, buf) -> sender @@ (to_req_plp_set id buf)
                             
  let send_event (type a) sender (msg : a event_request) : unit Lwt.t =
    (* no instant msgs *)
    match msg with
    | Measure id -> sender @@ to_req_measure id
    | Plps    id -> sender @@ to_req_plp_list id
                           
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
    let period       = 5 * (int_of_float (1. /. step_duration)) in
    let push_events  = event_push push_events in
    let detect_pool  = Msg_pool.create period (detect_msgs (send_msg sender)) in

    let rec first_step () =
      Msg_pool.send detect_pool () |> ignore;
      `Continue (step_detect detect_pool None)
    
    and step_detect detect_pool acc recvd =
      try
        Lwt_io.printf "Detect step\n" |> ignore;
        let recvd = Board_meta.concat_acc acc recvd in
        let _, responses, acc = deserialize recvd in
        match Msg_pool.responsed detect_pool responses with
        | Some detect -> step_start_init (List.append
                                            (plp_probes (send_event sender) detect)
                                            (measure_probes (send_event sender) detect))
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
                `Continue (step_normal probes_pool None)
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
                        Msg_pool.send probes_pool () |> ignore;
                        `Continue (step_normal probes_pool acc)
            | false  -> let init_pool = Msg_pool.next init_pool in
                        Msg_pool.send init_pool () |> ignore;
                        `Continue (step_init init_pool probes acc))
      with Timeout -> first_step ()
                             
    and step_normal probes_pool acc recvd =
      Lwt_io.printf "Normal step\n" |> ignore;
      let recvd = Board_meta.concat_acc acc recvd in
      let events, responses, acc = deserialize recvd in

      try
        if not @@ Msg_queue.empty !msgs
        then begin match Msg_queue.responsed !msgs responses with
             | None    -> msgs := Msg_queue.step !msgs
             | Some () -> msgs := Msg_queue.next !msgs;
                          Msg_queue.send !msgs () |> ignore
             end;
        let probes_pool = if Msg_pool.empty probes_pool
                          then probes_pool
                          else (match Msg_pool.responsed probes_pool events with
                                | None    -> Msg_pool.step probes_pool
                                | Some () -> let probes_pool = Msg_pool.next probes_pool in
                                             Msg_pool.send probes_pool () |> ignore;
                                             probes_pool)
        in
        CCList.iter push_events events;
        `Continue (step_normal probes_pool acc)
      with Timeout -> Msg_queue.iter !msgs wakeup_timeout;
                      msgs := Msg_queue.create period [];
                      push_state `No_response;
                      (first_step ())
    in
    first_step ()
    
  let create sender push_state step_duration =
    let period = 5 * (int_of_float (1. /. step_duration)) in
    let measure, mpush = React.E.create () in
    let plps, ppush = React.E.create () in
    let (events : events) = { measure; plps } in
    let push_events = { measure = mpush; plps = ppush } in
    let msgs = ref (Msg_queue.create period []) in
    events,
    (send msgs sender),
    (step msgs sender step_duration push_state push_events)

end
