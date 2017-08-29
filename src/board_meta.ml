open Common.Hardware

type board = { handlers        : (module Api_handler.HANDLER) list
             ; receiver        : Cbuffer.t -> unit
             ; streams_signal  : (int * string) list React.signal option
             ; is_converter    : bool
             ; state           : < >
             }
   
module type BOARD =
  sig
    val create       : topo_board -> (Cbuffer.t -> unit Lwt.t) -> board
    val connect_db   : board -> Database.t -> board
  end
