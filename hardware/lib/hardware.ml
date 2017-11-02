open Common.Topology
open Meta_board
open Containers

module Settings = struct

  type t = topology
  let to_yojson = topology_to_yojson
  let of_yojson = topology_of_yojson
  let default   = []
  let domain    = "topology"

end

module Conf = Storage.Config.Make(Settings)
module Map  = CCMap.Make(CCInt)
            
type t = { boards : Meta_board.board Map.t
         ; usb    : Usb_device.t
         ; topo   : topology React.signal
         }

let create_board db usb (b:topo_board) boards path step_duration =
  let (module B : Meta_board.BOARD) =
    match b.typ, b.model, b.manufacturer, b.version with
    | DVB,   "rf",       "niitv",  1  -> (module Board_dvb_niit  : Meta_board.BOARD)
    | IP2TS, "dtm-3200", "dektec", 1  -> (module Board_ip_dektec : Meta_board.BOARD)
    | TS,    "qos",      "niitv",  1  -> (module Board_qos_niit  : Meta_board.BOARD)
    (* | IP, "ts2ip", "niitv"        -> Board_ip.create version*)
    | _ -> raise (Failure ("create board: unknown board "))
  in
  B.create b
    (Meta_board.merge_streams boards)
    (Usb_device.get_send usb b.control)
    db path step_duration

let topo_inputs =
  let rec f = (fun acc entry -> match entry with
                                   | Input x -> x :: acc
                                   | Board x -> List.concat @@ (List.map (fun x -> f acc x.child) x.ports)) in
  List.fold_left f []

let topo_to_signal topo boards =
  let build_board b connection ports =
    React.S.l2 (fun a p -> Board { b with connection = a; ports = p }) connection ports
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
       let bstate    = Map.get b.control boards in
       let connection, port_list =
         match bstate with
         | None       -> React.S.const `No_response,
                         fun _ -> React.S.const false
         | Some state -> state.connection,
                         fun p -> Ports.get_or p state.ports_active ~default:(React.S.const false)
       in
       let ports = merge_ports @@
                     List.map (fun p -> p.port,
                                        port_list p.port,
                                        board_to_signal p.child)
                              b.ports
       in build_board b connection ports
  in
  List.map board_to_signal topo
  |> React.S.merge (fun acc h -> h::acc) []

  (*
let transform_streams topo_board boards (streams : Common.Stream.stream list) =
  let open CCOpt.Infix in
  
  let rec for_streams (s : Common.Stream.stream) =
    match s.source with
    | Port n    -> (List.hd topo_board
                    |> List.find_pred (fun p -> p.port = n)
                    >>= fun p ->
                    match p.child with
                    | Input i -> Some ({ source = (Input i : Topology.source)
                                      ; id     = (s.id :> [< `Ip_external | `Ip of Common.Stream.addr | `Ts of id])
                                      ; description = s.description
                                   } : Common.Stream.t)
                    | Board b -> (Map.get b
                                  >>= fun b ->
                                  b.
    | Stream id -> (CCList.find_pred (fun (s : Common.Stream.stream) -> match s.id with `Ts i -> i = id | _ -> false) streams
                      >>= fun s ->
                      for_streams s streams)
   *)
let create config db =
  let step_duration = 0.01 in
  let topo      = Conf.get config in
  let stor      = Storage.Options.Conf.get config in
  let usb, loop = Usb_device.create ~sleep:step_duration () in
  let rec traverse acc = (function
                          | Board b -> List.fold_left (fun a x -> traverse a x.child) (b :: acc) b.ports
                          | Input _ -> acc) in
  let boards = List.fold_left traverse [] topo (* TODO; Attention: traverse order now matters; 
                                                  child nodes come before parents *)
               |> List.fold_left (fun m b -> let board = create_board db usb b m stor.config_dir step_duration in
                                             Usb_device.subscribe usb b.control board.step;
                                             Map.add b.control board m)
                    Map.empty
  in
  let topo_signal = topo_to_signal topo boards in
  { boards; usb; topo = topo_signal }, loop ()

let finalize hw =
  Usb_device.finalize hw.usb
