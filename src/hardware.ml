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
         ; topo   : topology React.signal
         }
            
let create_adapter typ model manufacturer version =
  match typ, model, manufacturer with
  | DVB, "rf", "niitv"       -> Board_dvb.create version
  (* | IP, "dtm-3200", "dektec" -> Board_ts2ip.create version*)
  (* | TS, "qos", "niitv"       -> Board_qos.create version*)
  | _ -> raise (Failure ("create board: unknown board "))

let create_converter typ model manufacturer _ =
  match typ, model, manufacturer with
  (*| IP, "ts2ip", "niitv"  -> Board_ip.create version*)
  | _ -> raise (Failure ("create board: unknown board "))

            
let create_board db usb (b:topo_board)  =
  let (module B : BOARD) = 
    match b.typ with
    | Adapter   t -> create_adapter t b.model b.manufacturer b.version
    | Converter t -> create_converter t b.model b.manufacturer b.version
  in
  let board = B.create b (Usb_device.get_send usb b.control) in
  B.connect_db board db

let topo_to_signal topo boards =
  let build_board b active ports =
    React.S.l2 (fun a p -> Board { b with active = a; ports = p }) active ports
  in
  let merge_ports lst =
    List.map (fun (port, list, child) ->
        React.S.l2 (fun l c -> { port = port; listening = l; child = c }) list child)
             lst
    |> React.S.merge (fun acc h -> h::acc) []
  in
  let rec board_to_signal = function
    | Input _ as i -> React.S.const i
    | Board b ->
       let bstate    = List.find_pred (fun x -> Int.equal x.control b.control) boards in
       let active, port_list =
         match bstate with
         | None       -> React.S.const false,
                         fun _ -> React.S.const false
         | Some state -> state.is_active,
                         fun p -> Ports.get_or p state.ports_active ~default:(React.S.const false)
       in
       let ports = merge_ports @@
                     List.map (fun p -> p.port,
                                        port_list p.port,
                                        board_to_signal p.child)
                              b.ports
       in build_board b active ports
  in
  List.map board_to_signal topo
  |> React.S.merge (fun acc h -> h::acc) []
  
let create config db =
  let topo      = Conf.get config in
  let usb, loop = Usb_device.create () in
  let rec traverse acc = (function
                          | Board b -> List.fold_left (fun a x -> traverse a x.child) (b :: acc) b.ports
                          | Input _ -> acc) in
  let boards = List.fold_left traverse [] topo
               |> List.map (fun b -> let board = create_board db usb b in
                                     Usb_device.subscribe usb b.control board.step; board)
  in
  let topo_signal = topo_to_signal topo boards in
  { boards; usb; topo = topo_signal }, loop ()

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
