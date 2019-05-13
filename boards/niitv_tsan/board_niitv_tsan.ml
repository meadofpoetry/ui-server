open Application_types
open Board_niitv_tsan_types
open Board_niitv_tsan_protocol
open Util_react
open Boards

let ( >>= ) = Lwt_result.( >>= )

module Config = Kv_v.RW(Board_settings)

let create_logger (b : Topology.topo_board) =
  let log_name = Board.log_name b in
  let log_src = Logs.Src.create log_name in
  match b.logs with
  | None -> Ok log_src
  | Some x ->
    match Logs.level_of_string x with
    | Ok x ->
      Logs.Src.set_level log_src x;
      Ok log_src
    | Error _ -> Error (`Unknown_log_level x)

let invalid_port board x =
  let prefix = Boards.Board.log_name board in
  let s = prefix ^ ": invalid port " ^ (string_of_int x) in
  raise (Boards.Board.Invalid_port s)

let ports_sync
    (board : Topology.topo_board)
    (input : input signal)
    (streams : Stream.t list signal) : bool signal Boards.Board.Ports.t =
  List.fold_left (fun acc (p : Topology.topo_port) ->
      match input_of_int p.port with
      | None -> invalid_port board p.port
      | Some i ->
        let f a b = match a, b with
          | x, _ :: _ when equal_input i x -> true
          | _ -> false in
        let s = S.l2 ~eq:(=) f input streams in
        Boards.Board.Ports.add p.port s acc)
    Boards.Board.Ports.empty board.ports

let ports_active
    (board : Topology.topo_board)
    (input : input signal) : bool signal Boards.Board.Ports.t =
  List.fold_left (fun acc (p : Topology.topo_port) ->
      match input_of_int p.port with
      | None -> invalid_port board p.port
      | Some i ->
        let s = S.map ~eq:(=) (equal_input i) input in
        Boards.Board.Ports.add p.port s acc)
    Boards.Board.Ports.empty board.ports

let get_input_source_from_env src (b : Topology.topo_board) =
  match Topology.Env.find_opt "input_source" b.env with
  | None -> None
  | Some s ->
    match int_of_string_opt s with
    | Some x -> Some x
    | None ->
      Logs.warn ~src (fun m ->
          m "Failed to parse input source value from environment: %s" s);
      None

let get_t2mi_source_from_env src (b : Topology.topo_board) =
  match Topology.Env.find_opt "t2mi_source" b.env with
  | None -> None
  | Some s ->
    match int_of_string_opt s with
    | Some x -> Some x
    | None ->
      Logs.warn ~src (fun m ->
          m "Failed to parse T2-MI source value from environment: %s" s);
      None

let update_config_with_env src (config : config) (b : Topology.topo_board) =
  let config = match get_input_source_from_env src b with
    | None -> config
    | Some input_source -> { config with input_source } in
  match get_t2mi_source_from_env src b with
  | None -> config
  | Some t2mi_source -> { config with t2mi_source }

(* let tick tm =
 *   let e, push = React.E.create () in
 *   let rec loop () =
 *     push (); Lwt_unix.sleep tm >>= loop
 *   in
 *   e, loop
 * 
 * let appeared_streams
 *     sources
 *     ~(past : Stream.t list)
 *     ~(pres : Stream.t list) =
 *   let open Common.Stream in
 *   let rec not_in_or_diff s = function
 *     | [] -> true
 *     | so :: _ when equal so s -> false
 *     | _ :: tl -> not_in_or_diff s tl
 *   in
 *   let appeared =
 *     List.fold_left (fun acc pres ->
 *         if not_in_or_diff pres past
 *         then (Board_protocol.is_incoming sources pres, pres) :: acc
 *         else acc) [] pres in
 *   appeared *)

let create (b : Topology.topo_board)
    (streams : Stream.t list React.signal)
    (convert_streams : Topology.topo_board ->
     Stream.Raw.t list React.signal ->
     Stream.t list React.signal)
    (send : Cstruct.t -> unit Lwt.t)
    (db : Db.t)
    (kv : Kv.RW.t) : (Board.t, [> Board.error]) Lwt_result.t =
  Lwt.return @@ create_logger b
  >>= fun (src : Logs.src) ->
  let default = update_config_with_env src Board_settings.default b in
  Config.create ~default kv ["board"; (string_of_int b.control)]
  >>= fun (cfg : config Kv_v.rw) -> Lwt.return (create_logger b)
  >>= fun (src : Logs.src) -> Protocol.create src send (convert_streams b) cfg
  >>= fun (api : Protocol.api) ->
  let state = object
    method finalize () = Lwt.return ()
  end in
  (* let sources = match b.sources with
   *   | None ->
   *     let s = log_name ^ ": no sources provided!" in
   *     raise (Board.Invalid_sources s)
   *   | Some x ->
   *     begin match init_of_yojson x with
   *       | Ok init -> init
   *       | Error s -> raise (Board.Invalid_sources s)
   *     end in
   * let conv = fun x -> convert_streams x b in
   * let storage =
   *   Config_storage.create base
   *     ["board"; (string_of_int b.control)] in
   * let ({ ts; t2mi; _ } as events), api, step =
   *   SM.create sources send storage step conv in
   * let db = Result.get_exn @@ Db.Conn.create db_conf b.control in
   * let handlers = Board_api.handlers b.control db sources api events in
   * let tick, tick_loop = tick 5. in
   * let open React in
   * (\* State *\)
   * Lwt.ignore_result @@ Db.Device.init db;
   * E.keep
   * @@ E.map_p (fun e -> Db.Device.bump db e)
   * @@ E.select [ S.changes events.device.state
   *             ; S.sample (fun _ e -> e) tick events.device.state ];
   * (\* Streams *\)
   * let streams_ev =
   *   S.sample (fun () sl -> `Active sl) tick events.streams in
   * let streams_diff =
   *   S.diff (fun pres past -> `New (appeared_streams sources ~past ~pres))
   *     events.streams in
   * E.(keep
   *    @@ map_s (function
   *        | `Active x -> Db.Streams.bump_streams db x
   *        | `New x -> Db.Streams.insert_streams db x)
   *    @@ select [streams_ev; streams_diff]);
   * Db.Ts_info.(Single.handle ~eq:Ts_info.equal ~insert ~bump db tick ts.info);
   * Db.Pids.(Coll.handle ~eq:Pid.equal ~insert ~bump db tick ts.pids);
   * Db.Services.(Coll.handle ~eq:Service.equal ~insert ~bump db tick ts.services);
   * Db.T2mi_info.(Coll.handle ~eq:T2mi_info.equal ~insert ~bump db tick t2mi.structures);
   * E.(keep @@ map_s (Db.Bitrate.insert db) ts.bitrates);
   * E.(keep @@ map_s (Db.Bitrate.insert_pids db) ts.bitrates);
   * (\* E.(keep @@ map_p (Db.Errors.insert ~is_ts:true  db) events.ts.errors); *\)
   * E.(keep @@ map_p (Db.Errors.insert ~is_ts:false db) events.t2mi.errors); *)
  (* let ports_sync = Port.sync b events in
   * let ports_active = Port.active b events in *)
  let board =
    { Board.
      http = Board_niitv_tsan_http.handlers b.control api
    ; ws = []
    ; templates = []
    ; control = b.control
    ; streams_signal = React.S.const []
    ; log_source = (fun _ -> React.E.never)
    ; loop = api.loop
    ; push_data = api.push_data
    ; connection = api.notifs.state
    ; ports_sync = ports_sync b (React.S.const ASI) (React.S.const [])
    ; ports_active = ports_active b (React.S.const ASI)
    ; stream_handler = None
    ; state = (state :> < finalize : unit -> unit Lwt.t >)
    } in
  Lwt.return_ok board
