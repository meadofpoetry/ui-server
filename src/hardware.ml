open Common.Hardware
open Board_meta
open Containers

module Settings = struct

  type t = topology
  let to_yojson = topology_to_yojson
  let of_yojson = topology_of_yojson
  let default   = []
  let domain    = "topology"

end

module Conf = Config.Make(Settings)

type t = { handlers : (module Api_handler.HANDLER) list
         ; streams  : (int * string) list React.signal option
         }
            
let create_adapter typ model manufacturer version =
  match typ, model, manufacturer with
  | DVB, "rf", "niitv"       -> Board_dvb.create version
  | IP, "dtm-3200", "dektec" -> Board_ts2ip.create version
  | TS, "qos", "niitv"       -> Board_qos.create version
  | _ -> raise (Failure ("create board: unknown board "))

let create_converter typ model manufacturer version =
  match typ, model, manufacturer with
  | IP, "ts2ip", "niitv"  -> Board_ip.create version
  | _ -> raise (Failure ("create board: unknown board "))

            
let create_board db usb (b:topo_board)  =
  let (module B : BOARD) = 
    match b.typ with
    | Adapter   t -> create_adapter t b.model b.manufacturer b.version
    | Converter t -> create_converter t b.model b.manufacturer b.version
  in
  let board    = B.create b (Usb_device.get_send usb b.control) in
  B.connect_db board db;
  Usb_device.subscribe usb b.control (B.get_receiver board);
  let handlers = B.get_handlers board in
  let streams  = B.get_streams_signal board in
  handlers, streams

let handlers hw = hw.handlers

let create config db =
  let topo      = Conf.get config in
  let usb, loop = Usb_device.create () in
  let rec traverse acc = (function
                          | Board b -> List.fold_left (fun a x -> traverse a x.child) (b :: acc) b.ports
                          | Input _ -> acc) in
  let merge_signals a b =
    match a with
    | None   -> b
    | Some a -> match b with None -> Some a | Some b -> Some (React.S.l2 (@) a b)
  in
  let create (acch, accs) b =
    let h,s = create_board db usb b in
    h @ acch, merge_signals accs s
  in 
  let (handlers, streams) = List.fold_left traverse [] topo
                            |> List.fold_left create ([],None)
  in
  { handlers; streams }, loop ()


let finalize _ =
  ()
