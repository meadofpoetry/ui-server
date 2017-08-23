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
  typ_to_yojson b.typ |> Yojson.Safe.to_string |> Lwt_io.printf "%s\n\n" |> ignore

let create config =
  let topology = Conf.get config |> topology_of_config in
  let rec f acc = (function
                   | Board b -> List.fold_left (fun a x -> f a x.child) (b :: acc) b.ports
                   | Input _ -> acc) in
  let boards = List.fold_left f [] topology in
  List.map create_board boards


let finalize _ =
  ()

module type BOARD =
  sig
    type t
    val create : unit -> t
  end

module type BOARD_EVENTFUL =
  sig
    include BOARD
  end
