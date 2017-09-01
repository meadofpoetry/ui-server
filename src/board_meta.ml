open Common.Hardware
open Lwt.Infix

type 'a cc = [`Continue of 'a]
   
type state = [ `Fine | `No_response]
type req_typ = [`Need_response | `Instant]

module type MSG_DESC = sig
  type resp
  type req
end

module type PROTOCOL = sig
  include MSG_DESC

  val init        : req
  val probes      : req list
  val period      : int (* quantums *)
  val serialize   : req -> req_typ * Cbuffer.t
  val deserialize : Cbuffer.t -> resp list * Cbuffer.t option
  val is_response : req -> resp -> resp option

end

module type MESSENGER = sig
  include MSG_DESC
  
  val create : (Cbuffer.t -> unit Lwt.t) ->
               (state -> unit) ->
               (resp -> unit) ->
               (req -> resp Lwt.t) * (Cbuffer.t list -> 'c cc as 'c) cc
end
  
module Make(P : PROTOCOL)
       : (MESSENGER with type resp := P.resp
                     and type req := P.req) = struct
  (* TODO XXX Warning; Stateful module *)
  
  let send_msg msgs sender msg waker =
    match P.serialize msg with
    | `Need_response, x -> msgs := CCArray.append !msgs [|(ref P.period, msg, waker)|];
                           sender x
    | `Instant, x       -> sender x
  
  let send_probes msgs sender () =
    let req = P.probes in
    let rec send' = function
      | []    -> Lwt.return_unit
      | x::xs -> send_msg msgs sender x None >>= fun () -> send' xs
    in
    send' req
   
  let send_init msgs sender () =
    let req = P.init in
    send_msg msgs sender req None
  
  let send msgs sender req =
    let t, w = Lwt.wait () in
    send_msg msgs sender req (Some w)
    >>= fun () -> t

  exception Timeout
  exception Found
  let step msgs send_init send_probes push_state push_event =
    
    let rec good_step acc recvd =
      let recvd = match acc with
        | Some acc -> Cbuffer.append acc (Cbuffer.concat recvd)
        | None     -> Cbuffer.concat recvd
      in
      let received, acc = P.deserialize recvd in
    
      let lookup (period, req, waker) =
        let msg = CCList.find_map (P.is_response req) received in
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
        `Continue (good_step acc)
      with
      | Timeout -> msgs := [||];
                   push_state `No_response;
                   `Continue (no_response_step acc)

    and no_response_step acc recvd =      
      let recvd = match acc with
        | Some acc -> Cbuffer.append acc (Cbuffer.concat recvd)
        | None     -> Cbuffer.concat recvd
      in
      let received, acc = P.deserialize recvd in

      let lookup (period, req, waker) =
        let msg = CCList.find_map (P.is_response req) received in
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
        `Continue (no_response_step acc)
      with
      | Found -> push_state `Fine;
                 msgs := [||];
                 `Continue (good_step acc)
                 
    in
    `Continue (no_response_step None)
    
  let create sender push_state push_event =
    let msgs = ref [||] in
    let send_init   = send_init msgs sender in
    let send_probes = send_probes msgs sender in
    (send msgs sender), (step msgs send_init send_probes push_state push_event)
    
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

  val handlers : int -> (req -> resp Lwt.t)
                 -> state React.signal -> resp React.event
                 -> (module Api_handler.HANDLER) list
end
           
module type BOARD = sig
  include MSG_DESC
  val create       : topo_board -> (Cbuffer.t -> unit Lwt.t) -> board
  val connect_db   : board -> Database.t -> board
end
