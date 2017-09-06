open Common.Hardware
open Lwt.Infix

type 'a cc = [`Continue of 'a]
   
type state = [ `Fine | `No_response]
type instant = unit
             
module type MSG_DESC = sig
  type event
  type response
  type _ request
end

module type PROTOCOL = sig
  include MSG_DESC

  val period       : int (* quantums *)

  val detect       : response request
  val init         : response request list
  val probes       : response  -> event request list
  val is_response  : 'a request -> 'a -> 'a option
  val serialize    : _ request -> Cbuffer.t
  val deserialize  : Cbuffer.t -> event list * response list * Cbuffer.t option
    
end

module type MESSENGER = sig
  include MSG_DESC
  
  val create : (Cbuffer.t -> unit Lwt.t) ->
               (state -> unit) ->
               (event -> unit) ->
               (response request -> response Lwt.t) *
                 (instant request -> instant Lwt.t) *
                   (Cbuffer.t list -> 'c cc as 'c) cc
end
  
module Make(P : PROTOCOL)
       : (MESSENGER with type event := P.event
                     and type response := P.response
                     and type 'a request := 'a P.request) = struct
  (* TODO XXX Warning; Stateful module *)

  let send_msg sender msg =
    sender (P.serialize msg)

  let send_instant sender (msg : instant P.request) =
    sender (P.serialize msg)

  let send_await msgs sender msg =
    let t, w = Lwt.wait () in
    msgs := CCArray.append !msgs [|(ref P.period, msg, w)|];
    sender (P.serialize msg)
    >>= fun () -> t               
  
  let send_probes events sender probes =
    let rec send' acc = function
      | []    -> Lwt.return acc
      | x::xs -> sender (P.serialize x)
                 >>= fun () -> send' ((ref P.period, x)::acc) xs
    in
    send' [] probes
    >>= fun lst ->
    events := CCArray.append !events (CCArray.of_list lst);
    Lwt.return_unit
    
  exception Timeout
  let wakeup_timeout = fun (_,_,waker) -> Lwt.wakeup_exn waker (Failure "timeout")

  let concat_acc acc recvd = match acc with
    | Some acc -> Cbuffer.append acc (Cbuffer.concat recvd)
    | None     -> Cbuffer.concat recvd

  let initial_timeout = -1
         
  let step msgs events sender push_state push_event =
    let send_detect () = fun () -> send_msg sender P.detect in
    let send_msg       = send_msg sender in
    let send_probes    = send_probes events sender in
   
    let rec step_detect timeout acc recvd =
      Lwt_io.printf "Detect step\n" |> ignore;
      let recvd = concat_acc acc recvd in
      let _, responses, acc = P.deserialize recvd in
      let det = CCList.find_map (P.is_response P.detect) responses in
      match det with
      | Some detect -> step_start_init (P.probes detect)
      | None        -> if timeout < 0
                       then (send_detect () |> ignore; `Continue (step_detect P.period acc))
                       else `Continue (step_detect (pred timeout) acc)

    and step_start_init probes =
      match P.init with
      | [] -> `Continue (step_normal probes None)
      | x::tl -> send_msg x |> ignore;
                 `Continue (step_init P.period x tl probes None)
 
    and step_init timeout prev_req reqs probes acc recvd =
      Lwt_io.printf "Init step\n" |> ignore;
      let recvd = concat_acc acc recvd in
      let _, responses, acc = P.deserialize recvd in
      let init = CCList.find_map (P.is_response prev_req) responses in
      match init with
      | None    -> if timeout < 0
                   then (step_detect initial_timeout None [])
                   else `Continue (step_init (pred timeout) prev_req reqs probes acc)
      | Some _  ->
         match reqs with
         | []    -> push_state `Fine;
                    `Continue (step_normal probes acc)
         | x::tl -> send_msg x |> ignore;
                    `Continue (step_init P.period x tl probes acc)
 
    and step_normal probes acc recvd =
      Lwt_io.printf "Normal step\n" |> ignore;
      let recvd = concat_acc acc recvd in
      let eventslst, responses, acc = P.deserialize recvd in

      let lookup_msg (period, req, waker) =
        let msg = CCList.find_map (P.is_response req) responses in
        match msg with
        | None     -> decr period;
                      if !period <= 0 then raise_notrace Timeout;
                      Some (period, req, waker)
        | Some msg -> Lwt.wakeup waker msg; None
      in
      let lookup_event (period, req) =
        let msg = CCList.find_map (P.is_response req) eventslst in
        match msg with
        | None   -> decr period;
                    if !period <= 0 then raise_notrace Timeout;
                    Some (period, req)
        | Some _ -> None
      in   
      try
        msgs := CCArray.filter_map lookup_msg !msgs; (* XXX ?? *)
        events := CCArray.filter_map lookup_event !events; (* XXX *)
        CCList.iter push_event eventslst;
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
    
  let create sender push_state push_event =
    let msgs = ref [||] in
    let events = ref [||] in
    (send_await msgs sender),
    (send_instant sender),
    (step msgs events sender push_state push_event)
   
end

let apply = function `Continue step -> step

module Streams = CCMap.Make(CCInt)

type board = { handlers        : (module Api_handler.HANDLER) list
             ; connection      : state React.signal
             ; streams_signal  : string Streams.t React.signal option
             ; step            : (Cbuffer.t list -> 'c cc as 'c) cc
             ; is_converter    : bool
             ; state           : < >
             } 

module type BOARD_API = sig
  include MSG_DESC

  val handlers : int
                 -> (response request -> response Lwt.t)
                 -> (instant request -> instant Lwt.t)
                 -> state React.signal -> event React.event
                 -> (module Api_handler.HANDLER) list
end
           
module type BOARD = sig
  include MSG_DESC
  val create       : topo_board -> (Cbuffer.t -> unit Lwt.t) -> board
  val connect_db   : board -> Database.t -> board
end
