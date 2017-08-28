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

type t = { x : int }
            
let create_adapter typ (b : topo_board) db usb =
  let (module B : BOARD_EVENTFUL) =
    match typ, b.model, b.manufacturer with
    | DVB, "rf", "niitv" -> Board_dvb.create b.version
    | IP, "dtm-3200", "dektec" -> Board_ts2ip.create b.version
    | TS, "qos", "niitv"  -> Board_qos.create b.version
    | _ -> raise (Failure ("create board: unknown board " ^ (topo_board_to_yojson b |> Yojson.Safe.to_string)))
  in
  let board    = B.create b (Usb_device.get_send usb b.control) in
  B.connect_db board db;
  Usb_device.subscribe usb b.control (B.get_receiver board);
  let handlers = B.get_handlers board in
  let streams  = B.get_streams_signal board in
  handlers, Some streams

let create_converter typ (b : topo_board) db usb =
  let (module B : BOARD) =
    match typ, b.model, b.manufacturer with
    | IP, "ts2ip", "niitv"  -> Board_ip.create b.version
    | _ -> raise (Failure ("create board: unknown board " ^ (topo_board_to_yojson b |> Yojson.Safe.to_string)))
  in
  let board    = B.create b (Usb_device.get_send usb b.control) in
  B.connect_db board db;
  Usb_device.subscribe usb b.control (B.get_receiver board);
  let handlers = B.get_handlers board in
  handlers, None
            
let create_board db usb (b:topo_board)  =
  match b.typ with
  | Adapter   t -> create_adapter t b db usb
  | Converter t -> create_converter t b db usb

let handlers hw = hw

let create config db =
  let topo      = Conf.get config in
  let usb, loop = Usb_device.create () in
  let rec f acc = (function
                   | Board b -> List.fold_left (fun a x -> f a x.child) (b :: acc) b.ports
                   | Input _ -> acc) in
  List.fold_left f [] topo
  |> List.map (create_board db usb) |> ignore;
  (), loop ()


let finalize _ =
  ()
