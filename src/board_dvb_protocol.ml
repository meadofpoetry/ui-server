open Common.Board.Dvb
open Lwt.Infix

include Board_dvb_parser
   
(* Board protocol implementation *)

type 'a probe = { send : (unit -> unit Lwt.t)
                ; pred : ('a -> bool)
                }
      
let period = 50

let (detect : _ request) = Devinfo

let (init : _ request list) = [ Settings (0, { mode = T2
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
                            
let measure_probes (send_ev : 'a event_request -> unit Lwt.t) config =
  List.map (fun x ->
      { send = (fun () -> send_ev @@ Measure x)
      ; pred = (function `Measure (id,_) when id = x -> true | _ -> false)
    })
    config.modules

let plp_probes (send_ev : 'a event_request -> unit Lwt.t) config =
  List.map (fun x ->
      { send = (fun () -> send_ev @@ Plps x)
      ; pred = (function `Plps (id,_) when id = x -> true | _ -> false)
    })
    config.modules                 
  
module SM = struct

  exception Timeout

  type event = [ `Measure of (int * Cbuffer.t)
               | `Plps    of (int * Cbuffer.t)
               ]
                           
  module Event_probe = struct
    type t = { timeout : int
             ; timer   : int
             ; point   : int
             ; reqs    : event probe array
             }

    let create tm lst = { timeout = tm; timer = tm; point = 0; reqs = CCArray.of_list lst }

    let current t = t.reqs.(t.point)

    let responsed t = CCList.exists (current t).pred

    let send t = (current t).send

    let step t = let tmr = pred t.timer in
                 if tmr <= 0 then raise_notrace Timeout
                 else { t with timer = tmr }

    let next t = { t with point = ((succ t.point) mod (Array.length t.reqs)); timer = t.timeout }
  end

  let wakeup_timeout = fun (_, f) -> f None |> ignore

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
    match msg with
    | Devinfo -> let t, w = Lwt.wait () in
                 let find_parse = find_parse_devinfo w in
                 msgs := CCArray.append !msgs [| ref period, find_parse |];
                 sender (to_req_devinfo false) >>= fun () -> t
    | Reset ->   let t, w = Lwt.wait () in
                 let find_parse = find_parse_reset w in
                 msgs := CCArray.append !msgs [| ref period, find_parse |];
                 sender (to_req_devinfo true) >>= fun () -> t
    | Settings (id, buf) -> let t, w = Lwt.wait () in
                            let find_parse = find_parse_settings w id in
                            msgs := CCArray.append !msgs [| ref period, find_parse |];
                            sender (to_req_settings id buf) >>= fun () -> t
    | Plp_setting (id, buf) -> let t, w = Lwt.wait () in
                               let find_parse = find_parse_plp_settings w id in
                               msgs := CCArray.append !msgs [| ref period, find_parse |];
                               sender (to_req_plp_set id buf) >>= fun () -> t
    
  let initial_timeout = -1
                      
  let step msgs sender push_state push_events =
    let push_events  = event_push push_events in
    let send_detect  = fun () -> send_msg sender detect in
    let find_parse_detect = CCList.find_map (function `Devinfo buf -> Some (of_rsp_devinfo_exn buf)
                                                    | _ -> None) in
    
    let rec step_detect timeout acc recvd =
      Lwt_io.printf "Detect step\n" |> ignore;
      let recvd = Board_meta.concat_acc acc recvd in
      let _, responses, acc = deserialize recvd in
      let det = find_parse_detect responses in
      match det with
      | Some detect -> step_start_init (List.append
                                          (plp_probes (send_event sender) detect)
                                          (measure_probes (send_event sender) detect))
      | _           -> if timeout < 0
                       then (send_detect () |> ignore; `Continue (step_detect period acc))
                       else `Continue (step_detect (pred timeout) acc)

    and step_start_init probes =
      match init with
      | [] -> let probes = Event_probe.create period probes in
              Event_probe.send probes () |> ignore;
              `Continue (step_normal probes None)
      | x::tl -> send_msg sender x |> ignore;
                 `Continue (step_init period x tl probes None)
                 
    and step_init timeout prev_req reqs probes acc recvd =
      Lwt_io.printf "Init step\n" |> ignore;
      let recvd = Board_meta.concat_acc acc recvd in
      let _, responses, acc = deserialize recvd in
      let init = CCList.find_pred (is_response prev_req) responses in
      match init with
      | None    -> if timeout < 0
                   then (step_detect initial_timeout None [])
                   else `Continue (step_init (pred timeout) prev_req reqs probes acc)
      | Some _  ->
         match reqs with
         | []    -> push_state `Fine;
                    let probes = Event_probe.create period probes in
                    Event_probe.send probes () |> ignore;
                    `Continue (step_normal probes acc)
         | x::tl -> send_msg sender x |> ignore;
                    `Continue (step_init period x tl probes acc)
                    
    and step_normal probes acc recvd =
      Lwt_io.printf "Normal step\n" |> ignore;
      let recvd = Board_meta.concat_acc acc recvd in
      let events, responses, acc = deserialize recvd in

      let lookup_msg (period, f) =
        let msg = f @@ Some responses in
        match msg with
        | None     -> decr period;
                      if !period <= 0 then raise_notrace Timeout;
                      Some (period, f)
        | Some ()  -> None
      in
      try
        msgs := CCArray.filter_map lookup_msg !msgs; (* XXX ?? *)
        let probes = if not @@ Event_probe.responsed probes events
                     then Event_probe.step probes
                     else let next = Event_probe.next probes in
                          ignore @@ Event_probe.send next ();
                          next
        in
        CCList.iter push_events events;
        `Continue (step_normal probes acc)
      with
      | Timeout -> Array.iter wakeup_timeout !msgs;
                   msgs := [||];
                   push_state `No_response;
                   (step_detect initial_timeout None [])
    in
    `Continue (step_detect initial_timeout None)
    
  let create sender push_state =
    let measure, mpush = React.E.create () in
    let plps, ppush = React.E.create () in
    let (events : events) = { measure; plps } in
    let push_events = { measure = mpush; plps = ppush } in
    let msgs = ref [||] in
    events,
    (send msgs sender),
    (step msgs sender push_state push_events)

end
