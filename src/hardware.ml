open Common.Hardware
open Board_types
open Containers

module Settings = struct

  type t = topology
  let to_yojson = topology_to_yojson
  let of_yojson = topology_of_yojson
  let default   = []
  let domain    = "topology"

end

module Conf = Config.Make(Settings)

let create_board (b:topo_board) =
  let module V : VERSION = (struct let ver = b.version end) in
  let f (module B : BOARD) =
    let board = B.create b in
    B.get_handlers board in
  match (b.typ, b.model, b.manufacturer) with
  | (Adapter DVB), "rf", "niitv"  ->
     let (module B : BOARD)  = Board_dvb.create b.version in
     f (module B)
  | (Adapter IP), "dtm-3200", "dektec" ->
     let module B : BOARD = Board_ts2ip.Make(V) in
     f (module B)
  | (Adapter TS), "qos", "niitv"  ->
     Lwt_io.printf "%d\n" b.control |> ignore;
     let module B : BOARD = Board_qos.Make(V) in
     f (module B)
  | (Converter IP), "ts2ip", "niitv"  ->
     let module B : BOARD = Board_ip.Make(V) in
     f (module B)
  | _ -> raise (Failure ("create board: unknown board " ^ (topo_board_to_yojson b |> Yojson.Safe.to_string)))

let handlers hw = hw

let create config =
  let topo = Conf.get config in
  let rec f acc = (function
                   | Board b -> List.fold_left (fun a x -> f a x.child) (b :: acc) b.ports
                   | Input _ -> acc) in
  List.fold_left f [] topo
  |> List.map create_board |> List.concat


let finalize _ =
  ()
