open Common.Hardware
open Board_types
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

let create_board (b:topo_board) =
  let module V : VERSION = (struct let ver = b.version end) in
  let f (module B : BOARD) =
    let board = B.create b in
    B.get_handlers board in
  match (b.typ, b.model, b.manufacturer) with
  | (Adapter DVB), "rf", "niitv"  ->
     let module B : BOARD  = Board_dvb.Make(V) in
     f (module B)
  | (Adapter IP), "dtm-3200", "dektec" ->
     let module B : BOARD = Board_ts2ip.Make(V) in
     f (module B)
  | (Adapter TS), "qos", "niitv"  ->
     Lwt_io.printf "%d\n" b.id |> ignore;
     let module B : BOARD = Board_qos.Make(V) in
     f (module B)
  | (Converter IP), "ts2ip", "niitv"  ->
     let module B : BOARD = Board_ip.Make(V) in
     f (module B)
  | _ -> raise (Failure ("create board: unknown board " ^ (topo_board_to_yojson b |> Yojson.Safe.to_string)))

let handlers hw = hw

let create config =
  let topo = Conf.get config |> topology_of_config in
  let rec f acc = (function
                   | Board b -> List.fold_left (fun a x -> f a x.child) (b :: acc) b.ports
                   | Input _ -> acc) in
  List.fold_left f [] topo
  |> List.map create_board |> List.concat


let finalize _ =
  ()
