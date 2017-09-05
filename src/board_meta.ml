open Common.Hardware
open Lwt.Infix

type 'a cc = [`Continue of 'a]
   
type state = [ `Fine | `No_response]
type instant = unit
             
module type MSG_DESC = sig
  type init
  type event
  type response
  type _ request
end

module type PROTOCOL = sig
  include MSG_DESC

  val period       : int (* quantums *)
  val init         : init request
  val probes       : init -> instant request list
  val is_instant   : _ request -> bool
  val is_response  : response request -> response -> response option
  val serialize    : _ request -> Cbuffer.t
  val deserialize  : Cbuffer.t -> init option * event list * response list * Cbuffer.t option
    
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
       : (MESSENGER with type init := P.init
                     and type event := P.event
                     and type response := P.response
                     and type 'a request := 'a P.request) = struct
  (* TODO XXX Warning; Stateful module *)

  let send_instant (sender : Cbuffer.t -> instant Lwt.t) msg =
    sender (P.serialize msg)

  let send_await msgs sender msg =
    let t, w = Lwt.wait () in
    msgs := CCArray.append !msgs [|(ref P.period, msg, w)|];
    sender (P.serialize msg)
    >>= fun () -> t

  let send_init sender () =
    let req = P.init in
    send_instant sender req
  
  let send_probes sender probes =
    let rec send' = function
      | []    -> Lwt.return_unit
      | x::xs -> send_instant sender x >>= fun () -> send' xs
    in
    send' probes

  exception Timeout
  let wakeup_timeout = fun (_,_,waker) -> Lwt.wakeup_exn waker (Failure "timeout")

  let concat_acc acc recvd = match acc with
    | Some acc -> Cbuffer.append acc (Cbuffer.concat recvd)
    | None     -> Cbuffer.concat recvd

  let initial_timeout = -1
          
  let step msgs send_init send_probes push_state push_event =
    
    let rec good_step acc probes recvd =
      (* Lwt_io.printf "Good step\n" |> ignore; *)
      let recvd = concat_acc acc recvd in
                
      let _, events, responses, acc = P.deserialize recvd in
    
      let lookup (period, req, waker) =
        let msg = CCList.find_map (P.is_response req) responses in
        match msg with
        | None     -> decr period;
                      if !period <= 0 then raise_notrace Timeout;
                      Some (period, req, waker)
        | Some msg -> Lwt.wakeup waker msg; None
      in
     
      try
        msgs := CCArray.filter_map lookup !msgs; (* XXX ?? *)
        CCList.iter push_event events;
        send_probes probes |> ignore;
        `Continue (good_step acc probes)
      with
      | Timeout -> Array.iter wakeup_timeout !msgs;
                   msgs := [||];
                   push_state `No_response;
                   `Continue (no_response_step acc initial_timeout)

    and no_response_step acc timeout recvd =
      (*Lwt_io.printf "No response step\n" |> ignore; *)
      let recvd = concat_acc acc recvd in
                
      let init, _, _, acc = P.deserialize recvd in
      match init with
      | Some init ->
         push_state `Fine;
         `Continue (good_step acc (P.probes init))
      | None      ->
         if timeout >= 0
         then `Continue (no_response_step acc (pred timeout))
         else (send_init () |> ignore;
               `Continue (no_response_step acc P.period))

    in
    `Continue (no_response_step None initial_timeout)
    
  let create sender push_state push_event =
    let msgs = ref [||] in
    let send_init   = send_init sender in
    let send_probes = send_probes sender in
    (send_await msgs sender),
    (send_instant sender),
    (step msgs send_init send_probes push_state push_event)
    
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
