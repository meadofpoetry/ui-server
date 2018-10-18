open Containers
open Boards
open Board_types
open Common

let ( % ) = Fun.( % )

module Data = struct
  type t = Board_types.config
  let default = Board_types.config_default
  let dump = Board_types.config_to_string
  let restore = Board_types.config_of_string
end

module Config_storage = Storage.Options.Make (Data)

let invalid_port prefix x =
  let s = prefix ^ ": invalid_port " ^ (string_of_int x) in
  raise (Board.Invalid_port s)

let get_sync_ports prefix streams (ports : Topology.topo_port list) =
  List.fold_left (fun acc (p : Topology.topo_port) ->
      (match p.port with
       | 0 -> React.S.map ~eq:Equal.bool (not % List.is_empty) streams
       | x -> invalid_port prefix x)
      |> fun x -> Board.Ports.add p.port x acc)
    Board.Ports.empty ports

let get_active_ports prefix (ports : Topology.topo_port list) =
  List.fold_left (fun acc (p : Topology.topo_port) ->
      (match p.port with
       | 0 -> React.S.const true
       | x -> invalid_port prefix x)
      |> fun x -> Board.Ports.add p.port x acc)
    Board.Ports.empty ports

let log_prefix control = Printf.sprintf "(Board IP2TS: %d) " control

let create (({ control; ports; _ } as b) : Topology.topo_board) _
      convert_streams send db_conf base step : Board.t =
  let log_name = Boards.Board.log_name b in
  let log_src = Logs.Src.create log_name in
  let () = Option.iter (fun x -> Logs.Src.set_level log_src
                                 @@ Some x) b.logs in
  let logs = Logs.src_log log_src in
  let storage  = Config_storage.create base
                   ["board"; (string_of_int control)] in
  let events, api, step =
    Board_protocol.SM.create logs send storage step in
  let handlers = Board_api.handlers control api events in
  let streams = convert_streams events.streams b in
  let state = object
      method finalize () = ()
    end
  in
  { handlers
  ; control
  ; streams_signal = streams
  ; step
  ; connection = events.state
  ; ports_sync = get_sync_ports log_name streams ports
  ; ports_active = get_active_ports log_name ports
  ; stream_handler = None
  ; state = (state :> < finalize : unit -> unit >)
  ; templates = None
  }
