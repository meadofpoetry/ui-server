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

type t = { boards : Board_meta.board list
         ; usb    : Usb_device.t
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
  let board = B.create b (Usb_device.get_send usb b.control) in
  B.connect_db board db
  (*
  let board        = B.create b (Usb_device.get_send usb b.control) in
  let write_events = B.connect_db board db in
  Usb_device.subscribe usb b.control (B.get_receiver board);
  let handlers     = B.get_handlers board in
  let streams      = B.get_streams_signal board in
  handlers, streams, write_events
   *)

let create config db =
  let topo      = Conf.get config in
  let usb, loop = Usb_device.create () in
  let rec traverse acc = (function
                          | Board b -> List.fold_left (fun a x -> traverse a x.child) (b :: acc) b.ports
                          | Input _ -> acc) in
  let boards = List.fold_left traverse [] topo
               |> List.map (fun b -> let board = create_board db usb b in
                                     Usb_device.subscribe usb b.control board.receiver; board)
  in
  { boards; usb}, loop ()

let handlers hw =
  let hls = List.fold_left (fun acc x -> x.handlers @ acc) [] hw.boards in
  [ Api_handler.add_layer "board" hls ]
  
(*
  
  let merge_signals a b =
    match a with
    | None   -> b
    | Some a -> match b with None -> Some a | Some b -> Some (React.S.l2 (@) a b)
  in
  let create (acch, accs, acce) b =
    let h,s,wevents = create_board db usb b in
    h @ acch, merge_signals accs s, (wevents::acce)
  in 
  List.fold_left traverse [] topo
  |> List.fold_left create ([],None,[])
  |> fun (handlers, streams, write_events) ->
     { handlers; streams; write_events }, loop ()
 *)

let has_converters hw =
  List.exists (fun b -> b.is_converter) hw.boards

let streams hw =
  let signals =
    let open Option in
    List.map (fun b -> b.streams_signal) hw.boards
    |> Option.sequence_l
    >|= React.S.merge ~eq:(Streams.equal String.equal)
                      (Streams.union (fun _ _ b -> Some b))
                      Streams.empty
  in
  match signals with
  | Some s -> if has_converters hw
              then Some s
              else None
  | None   -> if has_converters hw
              then Some (React.S.const Streams.empty)
              else None
  
let finalize hw =
  Usb_device.finalize hw.usb
