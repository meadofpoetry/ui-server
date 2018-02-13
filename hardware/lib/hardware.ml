open Containers
open Common.Topology
open Meta_board

module Settings = struct

  type t = topology
  let to_yojson = topology_to_yojson
  let of_yojson = topology_of_yojson
  let default   = []
  let domain    = "topology"

end

module Conf = Storage.Config.Make(Settings)
module Map  = CCMap.Make(Int)

type t = { boards        : Meta_board.board Map.t
         ; usb           : Usb_device.t
         ; topo          : topology React.signal
         ; input_sources : Common.Stream.source list React.signal
         }

let create_board db usb (b:topo_board) boards path step_duration =
  let (module B : Meta_board.BOARD) =
    match b.typ, b.model, b.manufacturer, b.version with
    | DVB,   "rf",       "niitv",  1  -> (module Board_dvb_niit   : Meta_board.BOARD)
    | IP2TS, "dtm-3200", "dektec", 1  -> (module Board_ip_dektec  : Meta_board.BOARD)
    | TS,    "qos",      "niitv",  1  -> (module Board_qos_niit   : Meta_board.BOARD)
    | TS2IP, "ts2ip",    "niitv",  1  -> (module Board_ts2ip_niit : Meta_board.BOARD)
    | _ -> raise (Failure ("create board: unknown board "))
  in
  B.create b
           (Meta_board.get_streams boards b)
           (Meta_board.merge_streams boards)
           (Usb_device.get_send usb b.control)
           db path step_duration

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

let input_sources topology boards : Common.Stream.source list React.signal =
  let open Option.Infix in
  let open React in
  let check_stream (s : Common.Stream.t) : Common.Stream.source option =
    match s.id with
    | `Ts _ -> None
    | `Ip _ -> Some (Parent s)
  in
  let grep boards = function
    | Input i -> S.const [ (Input i : Common.Stream.source) ]
    | Board b ->
       match Map.get b.control boards with
       | None   -> S.const []
       | Some b -> S.map (List.filter_map check_stream) b.streams_signal
  in
  List.map (grep boards) topology
  |> S.merge ~eq:(Equal.physical) (fun a v -> List.append v a) []

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
 (* let _e = React.E.map (fun s ->
               `List (List.map Common.Stream.to_yojson s)
               |> Yojson.Safe.pretty_to_string
               |> Lwt_io.printf "DVB sms: %s\n"
               |> ignore;) @@ React.S.changes sms in *)
  let topo_signal   = topo_to_signal topo boards in
  let input_sources = input_sources topo boards in
  { boards; usb; topo = topo_signal; input_sources }, loop ()

let finalize hw =
  Usb_device.finalize hw.usb
