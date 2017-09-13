open Common.Hardware
open Lwt.Infix

type 'a cc = [`Continue of 'a]
   
type state = [ `Fine | `No_response]

type _ request
                                        
module Streams = CCMap.Make(CCInt)
module Ports = CCMap.Make(CCInt)

type board = { handlers        : (module Api_handler.HANDLER) list
             ; control         : int
             ; connection      : state React.signal
             ; streams_signal  : string Streams.t React.signal option
             ; step            : (Cbuffer.t list -> 'c cc as 'c) cc
             ; is_converter    : bool
             ; is_active       : bool React.signal
             ; ports_active    : bool React.signal Ports.t
             ; state           : < >
             } 
           
module type BOARD = sig
  type _ request
  val create       : topo_board -> (Cbuffer.t -> unit Lwt.t) -> board
  val connect_db   : board -> Database.t -> board
end

exception Timeout

type ('a, 'b) msg = { send : (unit -> unit Lwt.t)
                    ; pred : ('a -> 'b option)
                    }

module Msg_pool = struct
  type ('a,'b) t = { timeout : int
                   ; timer   : int
                   ; point   : int
                   ; reqs    : ('a,'b) msg array
                   }
  let create tm lst = { timeout = tm; timer = tm; point = 0; reqs = CCArray.of_list lst }
  let append t msgs = { t with reqs = CCArray.append t.reqs msgs }
  let empty t = CCArray.length t.reqs = 0
  let current t = t.reqs.(t.point)
  let responsed t = CCList.find_map (current t).pred
  let send t = (current t).send
  let step t = let tmr = pred t.timer in
               if tmr <= 0 then raise_notrace Timeout
               else { t with timer = tmr }
  let next t = { t with point = ((succ t.point) mod (Array.length t.reqs)); timer = t.timeout }
  let last t = CCInt.equal t.point (CCArray.length t.reqs)
  let map t f = CCArray.map f t.reqs
  let iter t f = CCArray.iter f t.reqs
end

module Msg_queue = struct
  type ('a,'b) t = { timeout : int
                   ; timer   : int
                   ; reqs    : ('a,'b) msg CCFQueue.t
                   }
  let create tm lst = { timeout = tm; timer = tm; reqs = CCFQueue.of_list lst }
  let append t msg = { t with reqs = CCFQueue.snoc t.reqs msg }
  let empty t = CCFQueue.size t.reqs = 0
  let responsed t m = CCOpt.(CCFQueue.first t.reqs >>= fun head -> CCList.find_map head.pred m)
  let send t () = try (CCFQueue.first_exn t.reqs).send ()
                  with _ -> Lwt.return_unit
  let step t = let tmr = pred t.timer in
               if tmr <= 0 then raise_notrace Timeout
               else { t with timer = tmr }
  let next t = { t with timer = t.timeout; reqs = CCFQueue.tail t.reqs }
  let map t f = CCFQueue.map f t.reqs
  let iter t f = CCFQueue.iter f t.reqs
end 

let concat_acc acc recvd = match acc with
  | Some acc -> Cbuffer.append acc (Cbuffer.concat (List.rev recvd))
  | None     -> Cbuffer.concat (List.rev recvd)

let apply = function `Continue step -> step
