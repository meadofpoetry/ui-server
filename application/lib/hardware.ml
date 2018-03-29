open Containers
open Common.Topology
open Meta_board

module Map  = CCMap.Make(Int)

type url = string

type marker = [ `Input of input * int | `Board of int ]

type in_push = (url * Common.Stream.source) list -> unit
type input_control  = [ `Input of input * int * in_push | `Board of int * Meta_board.stream_handler ]
                    
type t = { boards   : Meta_board.board Map.t
         ; usb      : Usb_device.t
         ; topo     : Common.Topology.t React.signal
         ; sources  : input_control list
         ; streams  : (marker * (url option * Common.Stream.source) list) list React.signal
         }

let create_board db usb (b:topo_board) boards path step_duration =
  let (module B : Meta_board.BOARD) =
    match b.typ, b.model, b.manufacturer, b.version with
    | "DVB",   "rf",       "niitv",  1  -> (module Board_dvb_niit   : Meta_board.BOARD)
    | "IP2TS", "dtm-3200", "dektec", 1  -> (module Board_ip_dektec  : Meta_board.BOARD)
    | "TS",    "qos",      "niitv",  1  -> (module Board_qos_niit   : Meta_board.BOARD)
    | "TS2IP", "ts2ip",    "niitv",  1  -> (module Board_ts2ip_niit : Meta_board.BOARD)
    | _ -> raise (Failure ("create board: unknown board "))
  in
  B.create b
           (Meta_board.get_streams boards b)
           (Meta_board.merge_streams boards)
           (Usb_device.get_send usb b.control)
           db path step_duration

(* TODO do some refactoring later on *)
let topo_to_signal topo boards : Common.Topology.t React.signal =
  let build_board b connection ports =
    React.S.l2 (fun a p -> Board { b with connection = a; ports = p }) connection ports
  in
  let merge_ports lst =
    List.map (fun (port, list, child) ->
        React.S.l2 (fun l c -> { port = port; listening = l; child = c }) list child)
             lst
    |> React.S.merge (fun acc h -> h::acc) []
  in
  let rec entry_to_signal = function
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
                                        entry_to_signal p.child)
                              b.ports
       in build_board b connection ports
  in
  let merge_entries l = React.S.merge (fun acc h -> h::acc) [] @@ List.map entry_to_signal l in
  let interface_to_signal i = React.S.map (fun conn -> { i with conn }) @@ entry_to_signal i.conn in
  let cpu_to_signal c =
    c.ifaces
    |> List.map interface_to_signal
    |> React.S.merge (fun acc h -> h::acc) []
    |> React.S.map (fun ifaces -> { c with ifaces })
  in
  match topo with
  | `CPU c    -> React.S.map (fun x -> `CPU x) @@ cpu_to_signal c
  | `Boards l ->
     List.map (fun b -> Board b) l
     |> merge_entries
     |> React.S.map (fun x -> `Boards (List.filter_map (function
                                           | Input _ -> None
                                           | Board b -> Some b)
                                         x))
     

let get_sources topo boards =
  let rec get_sources' controls signals = function
    | []            -> controls, signals
    | (Input i)::tl ->
       let s, push  = React.S.create [] in
       let push' xs = push @@ List.map (fun (url,x) -> (Some url, x)) xs in
       let controls = (`Input (i.input,i.id,push'))::controls in
       let signals  = (React.S.map (fun s -> (`Input (i.input,i.id)), s) s)::signals in
       get_sources' controls signals tl
    | (Board b)::tl ->
       let stream_handler = Option.(Map.get b.control boards
                                    >>= fun b -> b.stream_handler)
       in
       match stream_handler with
       | None   -> get_sources' controls signals tl
       | Some h ->
          let controls = (`Board (b.control, h))::controls in
          let signals  = (React.S.map (fun s -> (`Board b.control), s) h#streams)::signals in
          get_sources' controls signals tl
  in
  let controls, signals = get_sources' [] [] topo in
  let signals' = React.S.merge ~eq:Equal.physical (fun acc s -> s::acc) [] signals in
  controls, signals'

let create config db (topo : Common.Topology.t) =
  let stor          = Storage.Options.Conf.get config in
  let step_duration = 0.01 in
  let usb, loop     = Usb_device.create ~sleep:step_duration () in
  let rec traverse acc = (function
                          | Board b -> List.fold_left (fun a x -> traverse a x.child) (b :: acc) b.ports
                          | Input _ -> acc) in
  let topo_entries = Common.Topology.get_entries topo in
  let boards = List.fold_left traverse [] topo_entries (* TODO; Attention: traverse order now matters; 
                                                child nodes come before parents *)
               |> List.fold_left (fun m b -> let board = create_board db usb b m stor.config_dir step_duration in
                                             Usb_device.subscribe usb b.control board.step;
                                             Map.add b.control board m)
                    Map.empty
  in
  let sources, streams = get_sources topo_entries boards in
  let topo    = topo_to_signal topo boards in
  { boards; usb; topo; sources; streams }, loop ()

let finalize hw =
  Usb_device.finalize hw.usb;
