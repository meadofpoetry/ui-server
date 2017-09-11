open Common.Board.Dvb
open Lwt.Infix

include Board_dvb_parser
   
(* Board protocol implementation *)
   
let period = 5

let (detect : _ request) = Devinfo

let (init : _ request list) = [ Settings (0, { mode = T2
                                             ; bw   = Bw8
                                             ; freq = 586000000l
                                             ; plp  = 0})
                                         (* ; Req_settings (1, { mode = T2 *)
                                         (*                    ; bw   = Bw8 *)
                                         (*                    ; freq = 586000000l *)
                                         (*                    ; plp  = 0}) *)
                                         (* ; Req_settings (2, { mode = T2 *)
                                         (*                    ; bw   = Bw8 *)
                                         (*                    ; freq = 586000000l *)
                                         (*                    ; plp  = 0}) *)
                                         (* ; Req_settings (3, { mode = T2 *)
                                         (*                    ; bw   = Bw8 *)
                                         (*                    ; freq = 586000000l *)
                                (*                    ; plp  = 0}) *)]

let probes config =
  List.map (fun x -> Measure x) config.modules

let is_response (type resp) (req : resp request) (resp : resp) =
  match req, resp with
  | Devinfo, Devinfo _                                      -> true
  | Reset , Ack                                             -> true
  | Settings (id, _), Settings (ids, _)       when id = ids -> true
  | Plp_setting (id, _), Plp_setting (ids, _) when id = ids -> true
  | _ -> false

let is_event (req: event request) (resp : event) =
  match req, resp with
  | Measure id, Measure (ids, _)  when id = ids -> true
  | Plps id, Plps (ids, _)        when id = ids -> true
  | _ -> false
(*
let pp (type a) (req: a request) =
  let s = (match req with
           | Req_devinfo -> "req devinfo"
           | Reset       -> "req reset"
           | Req_settings _ -> "req settings"
           | Req_measure  _ -> "req measure"
           | Req_plps     _ -> "req plps"
           | Req_plp_setting _ -> "req plp setting") in
  Lwt_io.printf "%s\n" s |> ignore
 *)
       
module SM = struct

  type push_events = { measure : (int * rsp_measure) -> unit
                     ; plps    : (int * rsp_plp_list) -> unit
                     }

  let event_push pe = function
    | (Measure v : event) -> pe.measure v
    | (Plps v    : event) -> pe.plps v
                  
  let send_msg sender msg  = sender @@ serialize msg
  
  let send (type a) msgs sender (msg : a request) : a Lwt.t =
    (* no instant msgs *)
    let t, w = Lwt.wait () in
    msgs := CCArray.append !msgs [|(ref period, msg, w)|];
    sender (serialize msg)
    >>= (fun () -> t)

  let send_event (type a) events sender (msg : event request) : a Lwt.t =
    (* no instant msgs *)
    let t, w = Lwt.wait () in
    events := CCArray.append !events [|(ref period, msg, w)|];
    sender (serialize msg)
    >>= fun () -> t
                
  let send_probes events sender probes =
    let rec send' acc = function
      | []    -> Lwt.return acc
      | x::xs -> sender (serialize x)
                 >>= fun () -> send' ((ref period, x)::acc) xs
    in
    send' [] probes
    >>= fun lst ->
    events := CCArray.append !events (CCArray.of_list lst);
    Lwt.return_unit
    
  exception Timeout
  let wakeup_timeout = fun (_,_,waker) -> Lwt.wakeup_exn waker (Failure "timeout")

  let initial_timeout = -1
                      
  let step msgs sender push_state push_events =
    let events       = ref [||] in
    let push_events  = event_push push_events in
    let send_detect  = fun () -> send_msg sender detect in
    let send_probes  = send_probes events sender in
    
    let rec step_detect timeout acc recvd =
      Lwt_io.printf "Detect step\n" |> ignore;
      let recvd = Board_meta.concat_acc acc recvd in
      let _, responses, acc = deserialize recvd in
      let det = CCList.find_pred (is_response detect) responses in
      match det with
      | Some (Devinfo detect) -> step_start_init (probes detect)
      | _                     -> if timeout < 0
                                 then (send_detect () |> ignore; `Continue (step_detect period acc))
                                 else `Continue (step_detect (pred timeout) acc)

    and step_start_init probes =
      match init with
      | [] -> `Continue (step_normal probes None)
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
                    `Continue (step_normal probes acc)
         | x::tl -> send_msg sender x |> ignore;
                    `Continue (step_init period x tl probes acc)
                    
    and step_normal probes acc recvd =
      Lwt_io.printf "Normal step\n" |> ignore;
      let recvd = Board_meta.concat_acc acc recvd in
      let eventslst, responses, acc = deserialize recvd in

      let lookup_msg (period, req, waker) =
        let msg = CCList.find_pred (is_response req) responses in
        match msg with
        | None     -> decr period;
                      if !period <= 0 then (Lwt_io.printf "msg\n" |> ignore; raise_notrace Timeout);
                      Some (period, req, waker)
        | Some msg -> Lwt.wakeup waker msg; None
      in
      let lookup_event (period, req) =
        let msg = CCList.find_pred (is_event req) eventslst in
        match msg with
        | None   -> decr period;
                    if !period <= 0 then raise_notrace Timeout;
                    Some (period, req)
        | Some _ -> None
      in   
      try
        msgs := CCArray.filter_map lookup_msg !msgs; (* XXX ?? *)
        events := CCArray.filter_map lookup_event !events; (* XXX *)
        CCList.iter push_events eventslst;
        send_probes probes |> ignore; 
        `Continue (step_normal probes acc)
      with
      | Timeout -> Array.iter wakeup_timeout !msgs;
                   msgs := [||];
                   events := [||];
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
