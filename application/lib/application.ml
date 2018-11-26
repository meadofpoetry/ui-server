open Containers
open Common.React

module Settings_topology = struct
  include Common.Topology
  let default = `Boards []
  let domain = "topology"
end

module Conf_topology = Storage.Config.Make(Settings_topology)

type t =
  { proc : Data_processor.t option
  ; network : Pc_control.Network.t
  ; users : User.entries
  ; hw : Hardware.t
  ; db : Database.Conn.t
  ; topo : Common.Topology.t signal
  }

let proc_table = Data_processor.create_dispatcher [(module Pipeline)]

let filter_stream_table =
  let open Common.Stream.Table in
  List.filter_map (function
      | ({ url = None; _ } : stream) -> None
      | { url = Some uri; stream; _ } -> Some (uri, stream))
               
let create config db =
  let topology   = match Conf_topology.get_opt config with
    | None -> failwith "bad topology config"
    | Some t -> t
  in
  let users = User.create config in
  let options = Storage.Options.Conf.get config in
  let network = match Pc_control.Network.create config with
    | Ok net -> net
    | Error e -> failwith ("bad network config: " ^ e) in
  let proc = match topology with
    | `Boards bs -> None
    | `CPU c -> Data_processor.create proc_table c.process config db in
  let hw, loop = Hardware.create config db topology in
  let db = Result.get_exn @@ Database.Conn.create db () in
  (* Attach the process' reset mechanism to the stream_table signal 
     containing uris of the streams being measured *)
  Option.iter (fun (proc : Data_processor.t) ->
      hw.streams
      |> S.limit ~eq:Application_types.equal_stream_table (fun () ->
             Lwt_unix.sleep 2.)
      |> S.map ~eq:Equal.poly (fun (l : Application_types.stream_table) ->
             let open Common.Stream in
             List.fold_left (fun acc (_, _, ss) -> (filter_stream_table ss) @ acc) [] l
             |> List.sort (fun (_,l) (_,r) -> ID.compare l.id r.id))
      |> S.map ~eq:Equal.poly
             proc#reset
      |> S.keep) proc;
  (* Attach database to the aggregated log event stream *)
  hw.boards
  |> Hardware.Map.to_list
  (* Get boards' logs *)
  |> List.map (fun (_,b) -> Boards.Board.(b.log_source `All))
  (* Add proc's logs *)
  |> List.append (Option.map_or ~default:[] (fun p -> [p#log_source `All])
                    proc)
  |> Storage.Database.aggregate 1.0
  |> E.map_p (fun x -> Database.Log.insert db @@ List.concat x)
  |> E.keep;

  { users; proc; network; hw; db; topo = hw.topo }, loop

let redirect_filter app =
  Api.Redirect.redirect_auth (User.validate app.users)

let streams_on_input app input =
  let open Common in
  let open Boards.Board in
  let topo = S.value app.topo in
  match Topology.board_list_for_input input topo with
  | None -> Error "no such input"
  | Some boards ->
     try boards
         |> List.map (fun (topo_board : Topology.topo_board) ->
                let board = Hardware.Map.find topo_board.control app.hw.boards in
                topo_board.control, S.value board.streams_signal)
         |> Result.return
     with Not_found -> Error "internal topology error"
  
let stream_source app stream_id =
  let open Common in
  let open Boards.Board in
  let exception Res of Stream.source in
  let topo = S.value app.topo in
  try topo
      |> Topology.iter_boards (fun topo_board ->
             Hardware.Map.find_opt topo_board.control app.hw.boards
             |> Option.iter (fun board ->
                    S.value board.streams_signal
                    |> List.find_opt (fun s -> Stream.(ID.equal s.id stream_id))
                    |> function
                      | None -> ()
                      | Some s -> raise_notrace (Res s.source)));
      Error "not found"
  with Res src -> Ok src

let log_for_input app inputs stream_ids =
  let open Common in
  let open Boards.Board in
  let filter = match stream_ids with
    | [] -> `All
    | ids -> `Id ids
  in
  let topo = S.value app.topo in
  let inputs = match inputs with
    | [] -> Topology.get_inputs topo
    | l -> l in
  let boards =
    let sort_uniq =
      List.sort_uniq ~cmp:(fun (a : Topology.topo_board) b ->
          compare a.control b.control) in
    List.map (fun input ->
               match Topology.board_list_for_input input topo with
               | None -> Error "no such input"
               | Some bs -> Ok bs) inputs
    |> List.all_ok
    |> Result.map Fun.(sort_uniq % List.concat) in
  match boards with
  | Error e -> Error e
  | Ok boards ->
     try boards
         (* Get boards' log events *)
         |> List.map (fun (topo_board : Topology.topo_board) ->
                let board = Hardware.Map.find topo_board.control app.hw.boards in
                board.log_source filter)
         (* Add proc's log event *)
         |> List.append (Option.map_or ~default:[] (fun p -> [p#log_source filter])
                           app.proc)
         (* Merge *)
         |> Storage.Database.aggregate 0.5 (* TODO replace by something without sleep *)
         |> E.map List.concat
         |> Result.return
     with Not_found -> Error "internal topology error"
  
let finalize app =
  Hardware.finalize app.hw;
  Option.iter (fun p -> p#finalize ()) app.proc
