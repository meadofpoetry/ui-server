open Common.Hardware
open Lwt.Infix

module Streams = CCMap.Make(CCInt)

type board = { handlers        : (module Api_handler.HANDLER) list
             ; receiver        : Cbuffer.t -> unit
             ; streams_signal  : string Streams.t React.signal option
             ; is_converter    : bool
             ; state           : < >
             }
   
module type BOARD =
  sig
    val create       : topo_board -> (Cbuffer.t -> unit Lwt.t) -> board
    val connect_db   : board -> Database.t -> board
  end

module type PROTOCOL = sig
  type req
  type resp

  val init        : req
  val probes      : req list
  val period      : int (* quantums *)
  val deserialize : Cbuffer.t -> resp
  val serialize   : req -> Cbuffer.t
  val is_resp     : req -> resp -> bool
    
end
  
module Make(P : PROTOCOL) = struct
  (* TODO XXX Warning; Stateful module *)

  let send_probes msgs sender () =
    let req = P.probes in
    let rec send' = function
      | []    -> Lwt.return_unit
      | x::xs -> msgs := CCArray.append !msgs [|(ref P.period, x, None)|];
                 sender (P.serialize x) >>= fun () -> send' xs
    in
    send' req

  let send_init msgs sender () =
    let req = P.init in
    msgs := CCArray.append !msgs [|(ref P.period, req, None)|];
    sender (P.serialize req)
  
  let send msgs sender req =
    let buf  = P.serialize req in
    let t, w = Lwt.wait () in
    msgs := CCArray.append !msgs [|(ref P.period, req, Some w)|];
    sender buf >>= fun () ->
    t

  exception Timeout
  exception Found
  let step msgs send_init send_probes push_state push_event =
    
    let rec good_step recvd =
      let msg_pool = List.map P.deserialize recvd in
      
      let lookup (period, req, waker) =
        let msg = CCList.find_pred (P.is_resp req) msg_pool in
        match msg with
        | None     ->
           decr period;
           if !period <= 0 then raise_notrace Timeout;
           Some (period, req, waker)
        | Some msg -> 
           match waker with
           | None   -> push_event msg; None
           | Some w -> Lwt.wakeup w msg; None
      in
      try
        msgs := CCArray.filter_map lookup !msgs;
        send_probes () |> ignore;
        `Continue good_step
      with
      | Timeout -> msgs := [||];
                   push_state `No_response;
                   `Continue error_step

    and error_step recvd =
      let msg_pool = List.map P.deserialize recvd in

      let lookup (period, req, waker) =
        let msg = CCList.find_pred (P.is_resp req) msg_pool in
        match msg with
        | None     ->
           decr period;
           if !period <= 0
           then None
           else Some (period, req, waker)
        | Some _ -> raise_notrace Found 
      in

      try
        msgs := CCArray.filter_map lookup !msgs;
        send_init () |> ignore;
        `Continue error_step
      with
      | Found -> push_state `Fine;
                 msgs := [||];
                 `Continue good_step

    in
    good_step
    
  let create sender push_state push_event =
    let msgs = ref [||] in
    let send_init   = send_init msgs sender in
    let send_probes = send_probes msgs sender in
    (send msgs sender), `Continue (step msgs send_init send_probes push_state push_event)

end

let apply step recvd =
  match step with
  | `Continue step -> step recvd
