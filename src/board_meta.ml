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

let concat_acc acc recvd = match acc with
  | Some acc -> Cbuffer.append acc (Cbuffer.concat (List.rev recvd))
  | None     -> Cbuffer.concat (List.rev recvd)

let apply = function `Continue step -> step
