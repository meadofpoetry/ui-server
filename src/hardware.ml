open Common.Hardware
open Containers

type config = config_entry list [@@deriving yojson]
          
and config_entry  = Output of output
                  | Board  of typ * config_board
                            
and config_board = { control : int
                   ; version : version
                   ; ports   : config_port list
                   }
                 
and config_port = { port  : int
                  ; child : config_entry
                  }
                
let topology_of_config : config -> topology =
  let id = ref 0 in
  let rec of_entry : config_entry -> topo_entry = function
    | Output o      -> Output o
    | Board (t, bc) -> Board (of_board t bc)

  and of_board t bc =
    { id      = (id := !id + 1; !id)
    ; typ     = t
    ; version = bc.version 
    ; control = bc.control
    ; ports   = List.map of_port bc.ports
    }

  and of_port port =
    { port  = port.port
    ; child = of_entry port.child
    }
  in
  List.map of_entry
              
module type BOARD =
  sig
    type t
    val create : unit -> t
      
  end

module type BOARD_EVENTFUL =
  sig
    include BOARD
  end
