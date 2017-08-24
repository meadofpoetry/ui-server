open Common.Hardware
open Containers

module Settings = struct

  type t = config_entry list [@@deriving yojson]

   and config_entry  = Input of input
                     | Board  of typ * config_board
   and config_board = { control      : int
                      ; model        : string
                      ; manufacturer : string
                      ; version      : version
                      ; ports        : config_port list
                      }
   and config_port = { port  : int
                     ; child : config_entry
                     }
  let default = []
  let domain = "topology"

end

module Conf = Config.Make(Settings)

let topology_of_config : Settings.t -> topology =
  let id = ref 0 in
  let rec of_entry : Settings.config_entry -> topo_entry = function
    | Input i       -> Input i
    | Board (t, bc) -> Board (of_board t bc)

  and of_board t bc =
    { id           = (id := !id + 1; !id)
    ; typ          = t
    ; model        = bc.model
    ; manufacturer = bc.manufacturer
    ; version      = bc.version
    ; control      = bc.control
    ; ports        = List.map of_port bc.ports
    }

  and of_port port =
    { port  = port.port
    ; child = of_entry port.child
    }
  in
  List.map of_entry

let create_board b =
  match (b.typ, b.version, b.model, b.manufacturer) with
  | (Adapter DVB),  1, "rf",       "niitv"  -> Ok b
  | (Adapter IP),   3, "dtm-3200", "dektec" -> Ok b
  | (Adapter TS),   4, "qos",      "niitv"  -> Ok b
  | (Converter IP), 2, "ts2ip",    "niitv"  -> Ok b
  | _ -> Error ("create board: unknown board " ^ (topo_board_to_yojson b |> Yojson.Safe.to_string))

let create config =
  let topo = Conf.get config |> topology_of_config in
  let rec f acc = (function
                   | Board b -> List.fold_left (fun a x -> f a x.child) (b :: acc) b.ports
                   | Input _ -> acc) in
  List.fold_left f [] topo
  |> List.map create_board


let finalize _ =
  ()

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
