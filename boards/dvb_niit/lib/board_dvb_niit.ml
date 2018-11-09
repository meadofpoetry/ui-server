open Containers
open Boards
open Board_types
open Common

let ( % ) = Fun.( % )

module Api_handler = Api.Handler.Make(User)

module Data = struct
  open Device
  type t = config

  let default = []

  let dump c =
    Yojson.Safe.to_string @@ config_to_yojson c

  let restore s =
    config_of_yojson @@ Yojson.Safe.from_string s
end

module Config_storage = Storage.Options.Make(Data)

let invalid_port prefix port =
  let s = prefix ^ ": invalid port " ^ (string_of_int port) in
  raise (Board.Invalid_port s)

let rec has_sync = function
  | [] -> false
  | (_, ({ data; _ } : Measure.t Time.timestamped)) :: tl ->
     match data.lock, data.bitrate with
     | true, Some x when x > 0 -> true
     | _ -> has_sync tl

let create (b : Topology.topo_board) _ convert_streams send
      db_conf base step : Board.t =
  let log_name = Boards.Board.log_name b in
  let source_id = match b.sources with
    | None ->
       let s = log_name ^ ": no source provided!" in
       raise (Board.Invalid_sources s)
    | Some i ->
       begin match Json.Int.of_yojson i with
       | Ok i -> i
       | Error s -> raise (Board.Invalid_sources s)
       end in
  let log_src = Logs.Src.create log_name in
  Option.iter (fun x -> Logs.Src.set_level log_src @@ Some x) b.logs;
  let (module Logs : Logs.LOG) = Logs.src_log log_src in
  let module SM =
    Board_protocol.Make(Logs) in
  let storage = Config_storage.create base
                  ["board"; (string_of_int b.control)] in
  let db = Result.get_exn @@ Db.Conn.create db_conf b.control in
  let events, api, step =
    SM.create send (fun x -> convert_streams x b) source_id storage step in
  let handlers = Board_api.handlers b.control db api events in
  React.E.(keep @@ map_s (Db.Measurements.insert db) events.measures);
  let state = object
      method finalize () = ()
    end in
  { handlers
  ; control = b.control
  ; streams_signal = events.streams
  ; step
  ; connection = events.state
  ; ports_sync =
      List.fold_left (fun acc (p : Topology.topo_port) ->
          (match p.port with
           | 0 -> React.E.map has_sync events.measures
                  |> React.S.hold ~eq:Equal.bool false
           | x -> invalid_port log_name x)
          |> fun x -> Board.Ports.add p.port x acc)
        Board.Ports.empty b.ports
  ; ports_active =
      List.fold_left (fun acc (p : Topology.topo_port) ->
          (match p.port with
           | 0 -> React.S.const true
           | x -> invalid_port log_name x)
          |> fun x -> Board.Ports.add p.port x acc)
        Board.Ports.empty b.ports
  ; stream_handler = None
  ; state = (state :> < finalize : unit -> unit >)
  ; templates = None
  }
