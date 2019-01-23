open Containers
open Application_types
open Boards
open Common

module Map = CCMap.Make(Int)
module Uri_storage = Storage.Options.Make(External_uri_storage)

module Input_map =
  CCMap.Make(struct
      open Topology
      type t = input * int
      let compare (li, lid) (ri, rid) =
        let ci = compare_input li ri in
        if ci <> 0 then ci
        else Int.compare lid rid
    end)

type in_push = Stream.Table.setting list -> unit
type input_control =
  { inputs : in_push Input_map.t
  ; boards : Board.stream_handler Map.t
  }

type t =
  { boards : Boards.Board.t Map.t
  ; usb : Usb_device.t
  ; topo : Topology.t React.signal
  ; sources : input_control
  ; streams : stream_table React.signal
  ; uri_storage : Uri_storage.data Storage.Options.storage
  }

let create_board db usb (b : Topology.topo_board) boards path step_duration =
  let (module B : Board.BOARD) =
    match b.typ, b.model, b.manufacturer, b.version with
    | "DVB", "rf", "niitv", 1 -> (module Board_dvb_niit : Board.BOARD)
    | "IP2TS", "dtm-3200", "dektec", 1 -> (module Board_ip_dektec  : Board.BOARD)
    | "TS", "qos", "niitv", 1 -> (module Board_qos_niit : Board.BOARD)
    | "TS2IP", "ts2ip", "niitv", 1 -> (module Board_ts2ip_niit : Board.BOARD)
    | _ -> raise (Failure ("create board: unknown board ")) in
  B.create b
    (Board.get_streams boards b)
    (Board.merge_streams boards)
    (Usb_device.get_send usb b.control)
    db path step_duration

(* TODO do some refactoring later on *)
let topo_to_signal topo (boards : Board.t Map.t) : Topology.t React.signal =
  let open Topology in
  let cons l v = (Fun.flip List.cons) l v in
  let get_port m p = match Board.Ports.get p m with
    | None -> raise Not_found
    | Some p -> p in
  let build_board b connection ports =
    let eq = equal_topo_entry in
    React.S.l2 ~eq (fun a p -> Board { b with connection = a; ports = p })
      connection ports in
  let merge_ports lst =
    let eq = equal_topo_port in
    List.map (fun (port, sw, list, sync, child) ->
        React.S.l3 ~eq (fun l s c ->
            { port
            ; listening = l
            ; has_sync = s
            ; child = c
            ; switchable = sw }) list sync child) lst
    |> React.S.merge ~eq:(Equal.list eq) cons [] in
  let rec entry_to_signal = function
    | Input _ as i -> React.S.const i
    | Board b ->
       let connection, port_list, sync_list =
         match Map.get b.control boards with
         | None -> raise Not_found
         | Some state ->
            state.connection,
            (get_port state.ports_active),
            (get_port state.ports_sync) in
       let ports = merge_ports @@
                     List.map (fun p -> p.port,
                                        p.switchable,
                                        port_list p.port,
                                        sync_list p.port,
                                        entry_to_signal p.child)
                       b.ports
       in build_board b connection ports
  in
  let merge_entries l =
    let eq = Equal.list equal_topo_entry in
    React.S.merge ~eq cons []
    @@ List.map entry_to_signal l in
  let interface_to_signal i =
    React.S.map ~eq:equal_topo_interface (fun conn -> { i with conn })
    @@ entry_to_signal i.conn in
  let cpu_to_signal c =
    let eq = Equal.list equal_topo_interface in
    c.ifaces
    |> List.map interface_to_signal
    |> React.S.merge ~eq cons []
    |> React.S.map ~eq:equal_topo_cpu (fun ifaces -> { c with ifaces })
  in
  match topo with
  | `CPU c ->
     React.S.map ~eq:Topology.equal
       (fun x -> `CPU x) @@ cpu_to_signal c
  | `Boards l ->
     List.map (fun b -> Board b) l
     |> merge_entries
     |> React.S.map ~eq:Topology.equal (fun x ->
            `Boards (List.filter_map (function
                         | Input _ -> None
                         | Board b -> Some b)
                       x))

let get_sources (topo : Topology.topo_entry list) uri_table boards =
  let open Topology in
  let eq_row = equal_stream_table_row in
  let rec get_sources' controls signals = function
    | [] -> controls, signals
    | (Input i) :: tl ->
       let input = `Input (i.input, i.id) in
       let s, push  =
         let eq = Equal.list Stream.Table.equal_stream in
         (* consider previous uri preferences *)
         let (init : Stream.Table.stream list) =
           Option.(
             get_or ~default:[]
               (External_uri_storage.Map.find_opt input uri_table
                >|= List.map (fun (x : Stream.Table.setting) ->
                        Stream.Table.{ url = Some x.url
                                     ; stream = x.stream
                                     ; present = false }))) in
         React.S.create ~eq init in
       let push' xs =
         List.map (fun ({ url; stream } : Stream.Table.setting) ->
             Stream.Table.{ url = Some url
                          ; stream
                          ; present = false }) xs
         |> push in
       let controls = { controls with inputs = Input_map.add (i.input,i.id) push' controls.inputs } in
       let signals =
         (React.S.map ~eq:eq_row (fun s -> input, `Unlimited, s) s)
         :: signals in
       get_sources' controls signals tl
    | (Board b) :: tl ->
       let stream_handler =
         Option.(Map.get b.control boards
                 >>= fun (b : Board.t) -> b.stream_handler) in
       match stream_handler with
       | None -> get_sources' controls signals tl
       | Some h ->
          let board = `Board b.control in
          (* set previous uri preferences *)
          Option.iter (fun lst -> h#set lst |> ignore) @@
            External_uri_storage.Map.find_opt board uri_table;
          let controls = { controls with boards = Map.add b.control h controls.boards } in
          let signals =
            (React.S.l2 ~eq:eq_row (fun s st -> board, st, s)
               h#streams h#constraints.state)
            :: signals in
          get_sources' controls signals tl
  in
  let controls, signals =
    get_sources'
      { inputs = Input_map.empty
      ; boards = Map.empty }
      [] topo in
  let signals' =
    React.S.merge ~eq:equal_stream_table
      (fun acc s -> s :: acc) [] signals in
  controls, signals'

let store_external_uris storage streams =
  let open Stream.Table in
  let filter_streams =
    List.filter_map (function
        | ({ url = Some url; stream; _ } : stream) ->
           Some ({ url; stream } : setting)
        | _ -> None) in
  let streams =
    List.map (function
        | (`Board _ as b, _, l) -> b, filter_streams l
        | (`Input _ as i, _, l) -> i, filter_streams l)
      streams
  in
  storage#store @@ External_uri_storage.Map.of_list streams

let create config db (topo : Topology.t) =
  let open Topology in
  let stor = Storage.Options.Conf.get config in
  let uri_storage = Uri_storage.create stor.config_dir
                      ["application"; "uri_storage"] in
  let step_duration = 0.01 in
  let usb, loop = Usb_device.create ~sleep:step_duration () in
  let rec traverse acc = function
    | Board b ->
       List.fold_left (fun a x -> traverse a x.child)
         (b :: acc) b.ports
    | Input _ -> acc in
  let topo_entries = Topology.get_entries topo in
  let boards =
    List.fold_left traverse [] topo_entries (* TODO; Attention: traverse order now matters;
                                                child nodes come before parents *)
    |> List.fold_left (fun m b ->
           let board = create_board db usb b m stor.config_dir step_duration in
           Usb_device.subscribe usb b.control board.step;
           Map.add b.control board m)
         Map.empty
  in
  let sources, streams = get_sources topo_entries uri_storage#get boards in
  React.S.limit ~eq:equal_stream_table (fun () -> Lwt_unix.sleep 0.5) streams
  |> React.S.map ~eq:(fun _ _ -> false) (store_external_uris uri_storage)
  |> React.S.keep;
  let topo = topo_to_signal topo boards in
  { boards; usb; topo; sources; streams; uri_storage }, loop ()

exception Constraints of Stream.Table.set_error

let set_stream hw (ss : stream_setting) =
  let open Lwt_result.Infix in
  let open Stream.Table in
  let gen_uris (ss : stream_setting) : (marker * Stream.Table.setting list) list =
    let split = function
      | (`Input _, _) as i -> `Right i
      | (`Board id, sl)    ->
         try let p = Map.find id hw.sources.boards in
             let Board.{ range; state = _ } = p#constraints in
             `Left (List.map (fun s -> ((`Board id, s), range)) sl)
         with Not_found ->
           raise_notrace (Constraints (`Internal_error "set_stream: no control found"))
    in
    let rec rebuild_boards acc streams = function
      | [] -> acc
      | (`Input _, _) :: tl -> rebuild_boards acc streams tl
      | (`Board id, _) :: tl ->
         let same, rest =
           List.partition_map
             (function ((`Board bid, bs), buri) as v ->
                if id = bid
                then `Left { url = buri; stream = bs }
                else `Right v) streams in
         rebuild_boards ((`Board id, same) :: acc) rest tl in
    let input_add_uri (`Input i, sl) =
      let open Stream in
      let s_to_uri s = match s.orig_id with
        | TSoIP x ->
           let url = Url.{ ip = x.addr; port = x.port } in
           { url; stream = s }
        | _ -> raise_notrace (
                   Constraints (`Internal_error "set_stream: \
                                                 expected ip stream from an input")) in
      (`Input i, List.map s_to_uri sl) in
    let rec grep_input_uris acc = function
      | [] -> acc
      | (_, (sl : setting list)) :: tl ->
         let uris = List.map (fun x -> x.url) sl in
         grep_input_uris (uris @ acc) tl
    in
    (* TODO replace by a more generic check *)
    let check_inputs l =
      let rec check = function
        | [] -> ()
        | x::tl ->
           if List.exists (Stream.equal x) tl
           then raise_notrace (Constraints (`Internal_error "set_stream: input streams duplication"));
           check tl
      in List.iter (fun (_,l) -> check l) l
    in
    let boards, inputs = List.partition_map split ss in
    check_inputs inputs;
    let inputs = List.map input_add_uri inputs in
    let forbidden = grep_input_uris [] inputs in
    let boards = match Url.gen_in_ranges ~forbidden (List.concat boards) with
      | Ok boards -> rebuild_boards [] boards ss
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
    else List.iter (fun ({ url; _} : Stream.Table.setting) ->
             if not @@ List.exists (fun r -> Url.in_range r url) range
             then raise_notrace (Constraints `Not_in_range)) streams
  in
  let rec loop l _res = match l with
    | [] -> Lwt.return_ok ()
    | (`Input k, streams)::tl -> begin
        try let p = Input_map.find k hw.sources.inputs in
            p streams;
            (Lwt.return_ok ()) >>= loop tl
        with Not_found -> Lwt.return_error (`Internal_error "set_stream: no control found")
                          >>= loop tl
      end
    | (`Board k, streams) :: tl -> begin
        try let p = Map.find k hw.sources.boards in
            let Board.{ range; state } = p#constraints in
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
  Map.iter (fun _ (b : Board.t) -> b.state#finalize ()) hw.boards
