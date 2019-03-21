open Util_react

module User_api = User_api
   
let all_ok = List.fold_left (fun acc v ->
                 match acc with
                 | Error _ as e -> e
                 | Ok acc ->
                    match v with
                    | Error _ as e -> e
                    | Ok x -> Ok (x::acc))
               (Ok [])
                         
type t =
  { proc : Data_processor.t option
  ; network : Pc_control.Network.t
  ; users : User.passwd
  ; hw : Hardware.t
  ; db : Database.Conn.t
  ; topo : Application_types.Topology.t signal
  }

let proc_table = Data_processor.create_dispatcher [] (*(module Pipeline)]*)

let filter_stream_table =
  let open Application_types.Stream.Table in
  Hardware.filter_map (function
      | ({ url = None; _ } : stream) -> None
      | { url = Some uri; stream; _ } -> Some (uri, stream))
               
let create kv db =
  let (>>=) = Lwt_result.bind in

  Kv.RW.parse Application_types.Topology.of_string kv ["topology"]
  >>= fun topology ->
  
  User.create kv
  >>= fun users ->

  Pc_control.Network.create kv
  >>= fun network ->
(*
  let proc = match topology with
    | `Boards _ -> None
    | `CPU c -> Data_processor.create proc_table c.process config db in 
 *)
  Hardware.create kv db topology
  >>= fun (hw, loop) ->
  
  Database.Conn.create db ()
  >>= fun db ->
  
  (* Attach the process' reset mechanism to the stream_table signal 
     containing uris of the streams being measured *)
(*  Option.iter (fun (proc : Data_processor.t) ->
      hw.streams
      |> S.limit ~eq:Application_types.equal_stream_table (fun () ->
             Lwt_unix.sleep 2.)
      |> S.map ~eq:Equal.poly (fun (l : Application_types.stream_table) ->
             let open Common.Stream in
             List.fold_left (fun acc (_, _, ss) -> (filter_stream_table ss) @ acc) [] l
             |> List.sort (fun (_,l) (_,r) -> ID.compare l.id r.id))
      |> S.map ~eq:Equal.poly
             proc#reset
      |> S.keep) proc;*)
  (* Attach database to the aggregated log event stream *)
  hw.boards
  |> Hardware.Map.to_list
  (* Get boards' logs *)
  |> List.map (fun (_,b) -> Boards.Board.(b.log_source `All))
  (* Add proc's logs *)
 (* |> List.append (Option.map_or ~default:[] (fun p -> [p#log_source `All])
                    proc) *)
  |> Util_react.E.aggregate (fun () -> Lwt_unix.sleep 1.0)
  |> E.map_p (fun x -> Database.Log.insert db @@ List.concat x)
  |> E.keep;

  Lwt.return_ok ({ users; proc = None; (*proc;*) network; hw; db; topo = hw.topo }, loop)

let redirect_filter app =
  Api.Authorize.auth (User.validate app.users)
(*Api.Redirect.redirect_auth (User.validate app.users)*)

let streams_on_input app input =
  let open Application_types in
  let open Boards.Board in
  let topo = S.value app.topo in
  match Topology.board_list_for_input input topo with
  | None -> Error "no such input"
  | Some boards ->
     try boards
         |> List.map (fun (topo_board : Topology.topo_board) ->
                let board = Hardware.Map.find topo_board.control app.hw.boards in
                topo_board.control, S.value board.streams_signal)
         |> (fun v -> Ok v)
     with Not_found -> Error "internal topology error"
  
let stream_source app stream_id =
  let open Application_types in
  let open Boards.Board in
  let exception Res of Stream.source in
  let topo = S.value app.topo in
  try topo
      |> Topology.iter_boards (fun topo_board ->
             match Hardware.Map.find_opt topo_board.control app.hw.boards with
             | None -> ()
             | Some board ->
                S.value board.streams_signal
                |> List.find_opt (fun s -> Stream.(ID.equal s.id stream_id))
                |> function
                  | None -> ()
                  | Some s -> raise_notrace (Res s.source));
      Error "not found"
  with Res src -> Ok src

let log_for_input app inputs stream_ids =
  let open Application_types in
  let open Boards.Board in
  let filter = match stream_ids with
    | [] -> `All
    | ids -> `Id ids in
  let topo = S.value app.topo in
  let inputs = match inputs with
    | [] -> Topology.get_inputs topo
    | l -> l in
  let boards =
    let sort_uniq =
      List.sort_uniq (fun (a : Topology.topo_board) b ->
          compare a.control b.control) in
    List.map (fun input ->
               match Topology.board_list_for_input input topo with
               | None -> Error "no such input"
               | Some bs -> Ok bs) inputs
    |> all_ok
    |> (function Error _ as e -> e | Ok x -> Ok (sort_uniq @@ List.concat x))
  in
  match boards with
  | Error e -> Error e
  | Ok boards ->
     try boards
         (* Get boards' log events *)
         |> List.map (fun (topo_board : Topology.topo_board) ->
                let board = Hardware.Map.find topo_board.control app.hw.boards in
                board.log_source filter)
         (* Add proc's log event *)
       (*  |> List.append (Option.map_or ~default:[] (fun p -> [p#log_source filter])
                           app.proc) *)
         (* Merge *)
         |> E.aggregate (fun () -> Lwt_unix.sleep 0.5) (* TODO replace by something without sleep *)
         |> E.map List.concat
         |> (fun x -> Ok x)
     with Not_found -> Error "internal topology error"

let finalize app =
  Hardware.finalize app.hw
  (*Option.iter (fun p -> p#finalize ()) app.proc *)
 
