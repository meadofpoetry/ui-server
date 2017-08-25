open Common.Hardware

module type VERSION =
  sig
    val ver : int
  end

module type BOARD =
  sig
    type t
    val create       : topo_board -> t
    val connect_db   : t -> Database.t -> unit
    val get_handlers : t -> (module Api_handler.HANDLER) list
  end

module type BOARD_EVENTFUL =
  sig
    include BOARD
    val get_streams_signal : t -> (int * string) list React.signal
  end
