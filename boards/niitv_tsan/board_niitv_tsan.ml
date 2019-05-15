open Application_types
open Board_niitv_tsan_types
open Board_niitv_tsan_protocol
open Util_react
open Boards

let ( >>=? ) = Lwt_result.( >>= )
let ( >>= ) = Lwt.( >>= )

module Config = Kv_v.RW(Board_settings)

let create_log_src (b : Topology.topo_board) =
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

let create (b : Topology.topo_board)
    (streams : Stream.t list React.signal)
    (convert_streams : Topology.topo_board ->
     Stream.Raw.t list React.signal ->
     Stream.t list React.signal)
    (send : Cstruct.t -> unit Lwt.t)
    (db : Db.t)
    (kv : Kv.RW.t) : (Board.t, [> Board.error]) Lwt_result.t =
  Lwt.return @@ create_log_src b
  >>=? fun (src : Logs.src) ->
  let default = update_config_with_env src Board_settings.default b in
  Config.create ~default kv ["board"; (string_of_int b.control)]
  >>=? fun (kv : config Kv_v.rw) -> Protocol.create src send (convert_streams b) kv
  >>=? fun (api : Protocol.api) -> kv#get
  >>= fun config ->
  let state = object
    method finalize () = Lwt.return ()
  end in
  let input =
    React.S.hold ~eq:equal_input config.input
    @@ React.E.map (fun (s : Parser.Status.t) -> s.input) api.notifs.status in
  let board =
    { Board.
      http = Board_niitv_tsan_http.handlers b.control api
    ; ws = []
    ; templates = []
    ; control = b.control
    ; streams_signal = React.S.const []
    ; log_source = (fun src -> Board_logger.create b.control api src)
    ; loop = api.loop
    ; push_data = api.push_data
    ; connection = api.notifs.state
    ; ports_sync = ports_sync b input api.notifs.streams
    ; ports_active = ports_active b input
    ; stream_handler = None
    ; state = (state :> < finalize : unit -> unit Lwt.t >)
    } in
  Lwt.return_ok board
