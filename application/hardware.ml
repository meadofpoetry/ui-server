open Application_types
open Util_react
open Boards
open Netlib

(* Here be dragons, desperately needs a complete rewrite *)
(* TODO rewrite, especially the range constraints part *)

module Uri_storage = Kv_v.RW (External_uri_storage)

let equal_list compare l r =
  let sort_uniq = List.sort_uniq compare in
  let l = sort_uniq l in
  let r = sort_uniq r in
  try List.iter2 (fun l r -> if not (compare l r = 0) then raise_notrace Exit) l r;
      true
  with _ -> false

let partition_map f l =
  let rec loop accl accr = function
    | [] -> List.rev accl, List.rev accr
    | h :: tl ->
       match f h with
       | `Left x -> loop (x::accl) accr tl
       | `Right x -> loop accl (x::accr) tl
  in loop [] [] l

module Input_map =
  Map.Make(struct
    open Topology
    type t = input * int
    let compare (li, lid) (ri, rid) =
      let ci = compare_input li ri in
      if ci <> 0 then ci
      else Stdlib.compare lid rid
  end)

type in_push = Stream.Table.setting list -> unit

type input_control =
  { inputs : in_push Input_map.t
  ; boards : Board.stream_handler Board.Ports.t
  }

type t =
  { boards : Boards.Board.t Board.Ports.t
  ; usb : Usb_device.t option
  ; topo : Topology.t signal
  ; sources : input_control
  ; streams : Stream.stream_table signal
  ; uri_storage : Uri_storage.t
  }

let available_boards =
  [ (module Board_niitv_tsan : Board.BOARD)
  ; (module Board_niitv_ts2ip : Board.BOARD)
  ; (module Board_niitv_dvb4ch : Board.BOARD)
  ; (module Board_dektec_dtm3200 : Board.BOARD)
  ]

let create_board db usb (b : Topology.topo_board) boards kv =
  let id = Topology.board_id_of_topo_board b in
  let board = List.find_opt (fun board ->
      let (module B : Board.BOARD) = board in
      Topology.equal_board_id id B.board_id)
      available_boards
  in
  match board with
  | None -> raise (Failure ("create board: unknown board "))
  | Some board ->
    let (module B : Board.BOARD) = board in
    B.create b
      (Board.get_streams boards b)
      (Board.merge_streams boards)
      (Usb_device.get_send usb b.control)
      db kv

(* TODO do some refactoring later on *)
let topo_to_signal topo (boards : Board.t Board.Ports.t) : Topology.t React.signal =
  let open Topology in
  let cons l v = (Fun.flip List.cons) l v in
  let get_port m p = Board.Ports.find p m in
  let build_board b connection ports =
    let eq = equal_topo_entry in
    S.l2 ~eq (fun a p -> Board { b with connection = a; ports = p })
      connection ports in
  let merge_ports lst =
    let eq = equal_topo_port in
    List.map (fun (port, sw, list, sync, child) ->
        S.l3 ~eq (fun l s c ->
            { port
            ; listening = l
            ; has_sync = s
            ; child = c
            ; switchable = sw }) list sync child) lst
    |> S.merge ~eq:(equal_list Topology.compare_topo_port) cons [] in
  let rec entry_to_signal = function
    | Input _ as i -> React.S.const i
    | Board b ->
       let connection, port_list, sync_list =
         match Board.Ports.find_opt b.control boards with
         | None -> Printf.printf "board with control=%d not found\n" b.control; raise Not_found
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
    let eq = equal_list Topology.compare_topo_entry in
    React.S.merge ~eq cons []
    @@ List.map entry_to_signal l in
  let interface_to_signal i =
    React.S.map ~eq:equal_topo_interface (fun conn -> { i with conn })
    @@ entry_to_signal i.conn in
  let cpu_to_signal c =
    let eq = equal_list Topology.compare_topo_interface in
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
  let ( >|= ) o f = Option.map f o in
  let ( >>= ) = Option.bind in
  let eq_row = Stream.equal_stream_table_row in
  let rec get_sources' controls signals = function
    | [] -> controls, signals
    | (Input i) :: tl ->
       let input = `Input (i.input, i.id) in
       let s, push  =
         let eq = equal_list Stream.Table.compare_stream in
         (* consider previous uri preferences *)
         let (init : Stream.Table.stream list) =
           External_uri_storage.Map.find_opt input uri_table
           >|= List.map (fun (x : Stream.Table.setting) ->
                   Stream.Table.{ url = Some x.url
                                ; stream = x.stream
                                ; present = false })
           |> function None -> [] | Some x -> x
         in
         React.S.create ~eq init in
       let push' xs =
         List.map (fun ({ url; stream } : Stream.Table.setting) ->
             Stream.Table.{ url = Some url
                          ; stream
                          ; present = false }) xs
         |> push in
       let controls = { controls with inputs = Input_map.add (i.input,i.id) push' controls.inputs } in
       let signals =
         (S.map ~eq:eq_row (fun s -> input, `Unlimited, s) s)
         :: signals in
       get_sources' controls signals tl
    | (Board b) :: tl ->
       let stream_handler =
         Board.Ports.find_opt b.control boards
         >>= fun (b : Board.t) -> b.stream_handler in
       match stream_handler with
       | None -> get_sources' controls signals tl
       | Some h ->
          let board = `Board b.control in
          (* set previous uri preferences *)
          (* TODO proper Lwt type *)
          (* External_uri_storage.Map.find_opt board uri_table
           * >|= h#set
           * |> ignore; *)
          let controls = { controls with boards = Board.Ports.add b.control h controls.boards } in
          let signals =
            (React.S.l2 ~eq:eq_row (fun s st -> board, st, s)
               h#streams h#constraints.state)
            :: signals in
          get_sources' controls signals tl
  in
  let controls, signals =
    get_sources'
      { inputs = Input_map.empty
      ; boards = Board.Ports.empty }
      [] topo in
  let signals' =
    S.merge ~eq:Stream.equal_stream_table
      (fun acc s -> s :: acc) [] signals in
  controls, signals'

let store_external_uris (storage : Uri_storage.t) streams =
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
  List.to_seq streams
  |> External_uri_storage.Map.of_seq
  |> storage#set

let create kv db (topo : Topology.t) =
  let open Topology in
  (* TODO rempve in 4.08 *)
  let ( % ) f g x = f (g x) in
  let ( >>=? ) = Lwt_result.bind in
  let ( >>= ) = Lwt.bind in

  (* let rec traverse acc = function
   *   | Board b -> List.fold_left (fun a x -> traverse a x.child) (b :: acc) b.ports
   *   | Input _ -> acc in *)

  let step_duration = 0.01 in

  Uri_storage.create ~default:External_uri_storage.default
    kv ["application"; "uri_storage"]
  >>=? fun uri_storage ->
  let boards = get_boards topo in
  let topo_entries = Topology.get_entries topo in
  (match boards with
   | [] -> Lwt.return_ok (None, (fst % Lwt.wait), Board.Map.empty)
   | boards ->
     Usb_device.create ~sleep:step_duration ()
     >>= fun (usb, loop) ->
     Lwt_list.fold_left_s (fun m (b : topo_board) ->
         match m with
         | Error e -> Lwt.return_error e
         | Ok m ->
           create_board db usb b m kv
           >>=? fun (board : Board.t) ->
           Usb_device.subscribe usb b.control board.loop board.push_data;
           Lwt.return_ok @@ Board.Map.add b.control board m)
       (Ok Board.Map.empty) boards
     >>=? fun boards -> Lwt.return_ok (Some usb, loop, boards))
  >>=? fun (usb, loop, boards) -> uri_storage#get
  >>= fun uri_config ->
  let sources, streams = get_sources topo_entries uri_config boards in
  S.limit ~eq:Stream.equal_stream_table (fun () -> Lwt_unix.sleep 0.5) streams
  |> S.map ~eq:(fun _ _ -> false) (store_external_uris uri_storage)
  |> S.keep;
  let topo = topo_to_signal topo boards in
  Lwt.return_ok ({ boards; usb; topo; sources; streams; uri_storage }, loop ())

exception Constraints of Stream.Table.set_error

let set_stream ?(port=1234) (hw : t) (ss : Stream.stream_setting) =
  let open Lwt_result.Infix in
  let open Stream.Table in
  let gen_uris (ss : Stream.stream_setting)
      : (Stream.marker * Stream.Table.setting list) list =
    let split = function
      | (`Input _, _) as i -> `Right i
      | (`Board id, sl)    ->
         try let p = Board.Ports.find id hw.sources.boards in
             let Board.{ range; state = _ } = p#constraints in
             `Left (List.map (fun s -> ((`Board id, s), range)) sl)
         with Not_found ->
           raise_notrace (Constraints (`Internal_error "set_stream: no control found"))
    in
    let rec rebuild_boards port' acc streams = function
      | [] -> acc
      | (`Input _, _) :: tl -> rebuild_boards port' acc streams tl
      | (`Board id, _) :: tl ->
         let same, rest =
           partition_map
             (function ((`Board bid, bs), buri) as v ->
                if id = bid
                then
                  let url =
                    Uri.empty
                    |> (fun uri -> Uri.with_host uri (Some (Ipaddr.V4.to_string buri)))
                    |> (fun uri -> Uri.with_port uri (Some port'))
                    |> (fun uri -> Uri.with_scheme uri (Some "udp")) (* TODO proper scheme *)
                  in
                  `Left { url; stream = bs }
                else
                  `Right v) streams in
         rebuild_boards port' ((`Board id, same) :: acc) rest tl in
    let input_add_uri (`Input i, sl) =
      let open Stream in
      let s_to_uri s = match s.orig_id with
        | TSoIP x ->
           let url =
             Uri.empty
             |> (fun uri -> Uri.with_host uri (Some (Ipaddr.V4.to_string x.addr)))
             |> (fun uri -> Uri.with_port uri (Some x.port))
             |> (fun uri -> Uri.with_scheme uri (Some "udp")) (* TODO proper scheme *)
           in
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
    let boards, inputs = partition_map split ss in
    check_inputs inputs;
    let inputs = List.map input_add_uri inputs in
    let forbidden =
      grep_input_uris [] inputs
      |> List.filter_map Uri.host_v4
    in
    let boards = (*match Ipaddr.V4.gen_in_ranges ~forbidden (List.concat boards) with*)
      Ipaddr.V4.gen_in_ranges
        ~forbidden
        ~allowed:Netlib.Ipaddr.V4.multicast
        (List.concat boards)
      |> (fun boards -> rebuild_boards port [] boards ss)
                     (*Url.gen_in_ranges ~forbidden (List.concat boards) with*)
     (* | Ok boards -> rebuild_boards [] boards ss
      | Error ()  -> raise_notrace (Constraints (`Internal_error "set_stream: uri generation failure"))*)
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
    if range = [] then ()
    else List.iter (fun ({ url; _} : Stream.Table.setting) ->
             if not @@ List.exists (fun r ->
                           match Uri.host_v4 url with
                           | None -> false
                           | Some ip -> Ipaddr.V4.in_range r ip) range
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
        try let p = Board.Ports.find k hw.sources.boards in
            let Board.{ range = _; state } = p#constraints in
            let state = React.S.value state in
            check_constraints (*range*) [] state streams;
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
  let open Lwt.Infix in
  (match hw.usb with
   | None -> ()
   | Some usb -> Usb_device.finalize usb);
  Board.Map.fold
    (fun _ (b : Board.t) acc -> acc >>= b.state#finalize)
    hw.boards
    Lwt.return_unit

