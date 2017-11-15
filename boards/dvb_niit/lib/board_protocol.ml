open Board_types
open Lwt.Infix
open Storage.Options
open Api.Handler
open Meta_board
open Meta_board.Msg
   
include Board_parser
   
let ( % ) = CCFun.(%)
      
(* Board protocol implementation *)

let timeout_period step_duration = 2 * int_of_float (1. /. step_duration) (* 2 secs *)
          
let request_period step_duration = 5 * int_of_float (1. /. step_duration) (* 5 secs *)

let detect = Devinfo

let init = List.map (fun x -> Settings x)

let detect_msgs (send_req : 'a request -> unit Lwt.t) timeout =
  [ { send = (fun () -> send_req detect)
    ; pred = (is_response detect)
    ; timeout
    ; exn = None
  } ]

let init_msgs (send_req : 'a request -> unit Lwt.t) timeout d =
  List.map (fun x -> { send = (fun () -> send_req x)
                     ; pred = (is_response x)
                     ; timeout
                     ; exn = None })
    (init d)
  
let measure_probes (send_ev : 'a event_request -> unit Lwt.t) timeout config =
  List.map (fun x ->
      { send = (fun () -> send_ev @@ Measure x)
      ; pred = (is_event (Measure x))
      ; timeout
      ; exn = None
    })
    config.modules               
  
module SM = struct

  type event = [ `Measure of (int * Cbuffer.t) ]

  let wakeup_timeout t = t.pred `Timeout |> ignore

  type push_events = { measure : measure -> unit
                     }

  let event_push pe e = pe.measure e

  let send_msg (type a) sender (msg : a request) : unit Lwt.t =
    (match msg with
     | Devinfo               -> to_req_devinfo false
     | Reset                 -> to_req_devinfo true
     | Settings (id, buf)    -> to_req_settings id buf
     | Plp_setting (id, buf) -> to_req_plp_set id buf
     | Plps id               -> to_req_plp_list id)
    |> sender

  let send_event (type a) sender (msg : a event_request) : unit Lwt.t =
    (* no instant msgs *)
    match msg with
    | Measure id -> sender @@ to_req_measure id

  let send (type a) msgs sender (storage : config storage) timeout (msg : a request) : a Lwt.t =
    (* no instant msgs *)
    let t, w = Lwt.wait () in
    let pred = function
      | `Timeout -> Lwt.wakeup_exn w (Failure "msg timeout"); None
      | l -> let open CCOpt in
             is_response msg l >|= fun r ->
             (match msg with
              | Settings (d, dat) -> let conf = List.map (fun (i,old) -> if i = d
                                                                         then (d, dat)
                                                                         else (i, old))
                                                  storage#get
                                     in storage#store conf
              | _ -> ());
             Lwt.wakeup w r
    in
    let send = fun () -> send_msg sender msg in
    msgs := Queue.append !msgs { send; pred; timeout; exn = None };
    t

  let initial_timeout = -1

  let step msgs sender (storage : config storage) step_duration push_state push_events =
    let period         = timeout_period step_duration in
    let request_period = request_period step_duration in
    let push_events  = event_push push_events in
    let detect_pool  = Pool.create (detect_msgs (send_msg sender) period) in
    let time         = ref 0.0 in

    let rec first_step () =
      Queue.iter !msgs wakeup_timeout;
      msgs := Queue.create [];
      push_state `No_response;
      Pool.send detect_pool () |> ignore;
      `Continue (step_detect detect_pool None)

    and step_detect detect_pool acc recvd =
      try
        (*Lwt_io.printf "Detect step\n" |> ignore;*)
        let recvd = Meta_board.concat_acc acc recvd in
        let _, responses, acc = deserialize recvd in
        match Pool.responsed detect_pool responses with
        | Some detect -> step_start_init (measure_probes (send_event sender) period detect)
        | _           -> `Continue (step_detect (Pool.step detect_pool) acc)
      with Timeout -> first_step ()

    and step_start_init probes =
      try
        match init_msgs (send_msg sender) period storage#get with
        | [] -> let probes_pool = Pool.create probes in
                Pool.send probes_pool () |> ignore;
                `Continue (step_normal_probes_send probes_pool 0 None)
        | lst -> let init_pool = Pool.create lst in
                 Pool.send init_pool () |> ignore;
                 `Continue (step_init init_pool probes None)
      with Timeout -> first_step ()

    and step_init init_pool probes acc recvd =
      try
        (*Lwt_io.printf "Init step\n" |> ignore;*)
        let recvd = Meta_board.concat_acc acc recvd in
        let _, responses, acc = deserialize recvd in
        match Pool.responsed init_pool responses with
        | None    -> `Continue (step_init (Pool.step init_pool) probes acc)
        | Some _  ->
           (match Pool.last init_pool with
            | true   -> push_state `Fine;
                        let probes_pool = Pool.create probes in
                        `Continue (step_normal_probes_send probes_pool 0 acc)
            | false  -> let init_pool = Pool.next init_pool in
                        Pool.send init_pool () |> ignore;
                        `Continue (step_init init_pool probes acc))
      with Timeout -> first_step ()

    and step_normal_probes_send probes_pool period_timer acc _ =
      (*Lwt_io.printf "Normal step probes send \n" |> ignore;*)
      if (period_timer >= request_period) then raise (Failure "board_dvb: sm invariant is broken");

      if Pool.empty probes_pool
      then `Continue (step_normal_requests_send probes_pool (succ period_timer) acc)
      else (Pool.send probes_pool () |> ignore;
            Lwt_io.printlf "sending probe" |> ignore;
            time := Unix.gettimeofday ();
            `Continue (step_normal_probes_wait probes_pool (succ period_timer) acc))

    and step_normal_probes_wait probes_pool period_timer acc recvd =
      (*Lwt_io.printf "Normal step probes recv\n" |> ignore;*)
      let recvd_buf = Meta_board.concat_acc acc recvd in
      let events, _, acc = deserialize recvd_buf in

      try
        (match Pool.responsed probes_pool events with
         | None    -> let probes_pool = Pool.step probes_pool in
                      `Continue (step_normal_probes_wait probes_pool (succ period_timer) acc)
         | Some ev -> Lwt_io.printlf "received probe, %f" (Unix.gettimeofday () -. !time) |> ignore;
                      let new_probes_pool = Pool.next probes_pool in
                      push_events ev;
                      if Pool.last probes_pool
                      then `Continue (step_normal_requests_send new_probes_pool period_timer acc)
                      else step_normal_probes_send new_probes_pool period_timer acc recvd)
      with Timeout -> Lwt_io.printlf "probe not received, %f" (Unix.gettimeofday () -. !time)|> ignore; first_step ()

    and step_normal_requests_send probes_pool period_timer acc _ =
      (*Lwt_io.printf "Normal step requests send\n" |> ignore;*)
      if (period_timer >= request_period)
      then `Continue (step_normal_probes_send probes_pool ((succ period_timer) mod request_period) acc)
      else 
        if Queue.empty !msgs
        then `Continue (step_normal_requests_send probes_pool (succ period_timer) acc)
        else (Queue.send !msgs () |> ignore;
              `Continue (step_normal_requests_wait probes_pool (succ period_timer) acc))

    and step_normal_requests_wait probes_pool period_timer acc recvd =
      (*Lwt_io.printf "Normal step requests recv\n" |> ignore;*)
      let recvd = Meta_board.concat_acc acc recvd in
      let _, responses, acc = deserialize recvd in
      try
        match Queue.responsed !msgs responses with
        | None    -> msgs := Queue.step !msgs;
                     `Continue (step_normal_requests_wait probes_pool (succ period_timer) acc)
        | Some () -> msgs := Queue.next !msgs;
                     `Continue (step_normal_requests_send probes_pool (succ period_timer) acc)
      with Timeout -> first_step ()
    in
    first_step ()
    
  let create sender (storage : config storage) push_state step_duration =
    let period = timeout_period step_duration in
    let measure, mpush = React.E.create () in
    let (events : events) = { measure } in
    let push_events = { measure = mpush } in
    let msgs = ref (Queue.create []) in
    let send x = send msgs sender storage period x in
    let api = { devinfo     = (fun ()    -> send Devinfo)
              ; reset       = (fun ()    -> send Reset)
              ; settings    = (fun s     -> send (Settings s))
              ; plp_setting = (fun (n,s) -> send (Plp_setting (n,s)))
              ; plps        = (fun n     -> send (Plps n))
              ; config      = (fun ()    -> Lwt.return storage#get)
              }
    in
    events,
    api,
    (step msgs sender storage step_duration push_state push_events)

end
