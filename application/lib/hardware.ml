open Containers
open Common.Topology
open Meta_board
open Application_types
   
module Map  = CCMap.Make(Int)

module Input_map = CCMap.Make(struct
                       type t = input * int
                        let compare (li, lid) (ri, rid) =
                          let ci = input_compare li ri in
                          if ci <> 0 then ci
                          else compare lid rid
                     end)
         
type in_push = (url * Common.Stream.t) list -> unit
type input_control  = { inputs : in_push Input_map.t
                      ; boards : Meta_board.stream_handler Map.t
                      }
                    
type t = { boards   : Meta_board.board Map.t
         ; usb      : Usb_device.t
         ; topo     : Common.Topology.t React.signal
         ; sources  : input_control
         ; streams  : stream_table React.signal
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
       let controls = { controls with inputs = Input_map.add (i.input,i.id) push' controls.inputs } in
       let signals  = (React.S.map (fun s -> (`Input (i.input,i.id)), `Unlimited, s) s)
                      :: signals in
       get_sources' controls signals tl
    | (Board b)::tl ->
       let stream_handler = Option.(Map.get b.control boards
                                    >>= fun b -> b.stream_handler)
       in
       match stream_handler with
       | None   -> get_sources' controls signals tl
       | Some h ->
          let controls = { controls with boards = Map.add b.control h controls.boards } in
          let signals  = (React.S.l2 (fun s st -> (`Board b.control), st, s) h#streams h#constraints.state)
                         :: signals in
          get_sources' controls signals tl
  in
  let controls, signals = get_sources' { inputs = Input_map.empty; boards = Map.empty } [] topo in
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

exception Constraints of Meta_board.set_error

let set_stream hw (ss : stream_setting) =
  let open Lwt_result.Infix in
  let gen_uris (ss : stream_setting) : (marker * (Common.Uri.t * Common.Stream.t) list) list =
     let split = function
      | (`Input _, _) as i -> `Right i
      | (`Board id, sl)    ->
         try let p = Map.find id hw.sources.boards in
             let { range; state = _ } = p#constraints in
             `Left (List.map (fun s -> ((`Board id, s),range)) sl)
         with Not_found ->
           raise_notrace (Constraints (`Internal_error "set_stream: no control found"))
     in
     let rec rebuild acc = function
       | [] -> acc
       | ((`Board id, s), uri)::tl ->
          let same, rest = List.partition_map (function ((`Board bid,bs),buri) as v ->
                               if id = bid
                               then `Left  (buri,bs)
                               else `Right v
                             ) tl
          in
          rebuild ((`Board id, (uri,s)::same)::acc) rest
     in
     let rec input_add_uri (`Input i, sl) =
       let open Common.Stream in
       let s_to_uri s = match s.id with
         | `Ts _ -> raise_notrace (Constraints (`Internal_error "set_stream: expected ip stream from an input"))
         | `Ip u -> (u, s)
       in (`Input i, List.map s_to_uri sl)
     in
     let rec grep_input_uris acc = function
       | [] -> acc
       | (_, sl)::tl ->
          let uris = List.map fst sl in
          grep_input_uris (uris @ acc) tl
     in
     (* TODO replace by a more generic check *)
     let check_inputs l =
       let rec check = function
         | [] -> ()
         | x::tl ->
            if List.exists (Common.Stream.equal x) tl
            then raise_notrace (Constraints (`Internal_error "set_stream: input streams duplication"));
            check tl
       in List.iter (fun (_,l) -> check l) l
     in
     let boards, inputs = List.partition_map split ss in
     check_inputs inputs;
     let inputs = List.map input_add_uri inputs in
     let forbidden = grep_input_uris [] inputs in
     let boards = match Common.Uri.gen_in_ranges ~forbidden (List.concat boards) with
       | Ok boards -> rebuild [] boards
       | Error ()  -> raise_notrace (Constraints (`Internal_error "set_stream: uri generation failure"))
     in
     inputs @ boards
  in
  (* TODO simplify this part *)
  let check_constraints range state streams =
    begin match state with
    | `Forbidden -> raise_notrace (Constraints `Forbidden)
    | `Limited l ->
       let len = List.length streams in
       if l >= len then ()
       else raise_notrace (Constraints (`Limit_exceeded (l,len)))
    | _ -> ()
    end;
    if List.is_empty range then ()
    else List.iter (fun (url,_) -> if not @@ List.exists (fun r -> Common.Uri.in_range r url) range
                                   then raise_notrace (Constraints `Not_in_range)) streams
  in
  let rec loop l res = match l with
    | [] -> Lwt.return_ok ()
    | (`Input k, streams)::tl -> begin
        try let p = Input_map.find k hw.sources.inputs in
            p streams;
            (Lwt.return_ok ()) >>= loop tl
        with Not_found -> Lwt.return_error (`Internal_error "set_stream: no control found")
                          >>= loop tl
      end
    | (`Board k, streams)::tl -> begin
        try let p = Map.find k hw.sources.boards in
            let { range; state } = p#constraints in
            let state = React.S.value state in
            check_constraints range state streams;
            p#set streams >>= loop tl
        with Not_found -> Lwt.return_error (`Internal_error "set_stream: no control found")
                          >>= loop tl
           | Constraints e -> Lwt.return_error e >>= loop tl
      end
  in
  try
    let ss = gen_uris ss in
    loop ss ()
  with Constraints e -> Lwt.return_error e
      
let finalize hw =
  Usb_device.finalize hw.usb;
