open Common.Hardware

module type BOARD =
  sig
    type t
    val create       : topo_board -> (Cbuffer.t -> unit Lwt.t) -> t
    val connect_db   : t -> Database.t -> unit
    val get_handlers : t -> (module Api_handler.HANDLER) list
    val get_receiver : t -> (Cbuffer.t -> unit)
  end

module type BOARD_EVENTFUL =
  sig
    include BOARD
    val get_streams_signal : t -> (int * string) list React.signal
  end
