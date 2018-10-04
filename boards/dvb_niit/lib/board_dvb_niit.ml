open Containers
open Common.Topology
open Boards.Board
open Board_types

module Api_handler = Api.Handler.Make (Common.User)

module Data = struct
  open Device
  type t = config

  let default = []

  let dump c =
    Yojson.Safe.to_string @@ config_to_yojson c

  let restore s =
    config_of_yojson @@ Yojson.Safe.from_string s
end

module Config_storage = Storage.Options.Make (Data)

let invalid_port prefix port =
  let s = prefix ^ ": invalid port " ^ (string_of_int port) in
  raise (Invalid_port s)

let create (b : topo_board) _ convert_streams send _ (* db_conf*) base step =
  let log_name = Boards.Board.log_name b in
  let source_id = match b.sources with
    | None ->
       let s = log_name ^ ": no source provided!" in
       raise (Invalid_sources s)
    | Some i ->
       begin match Common.Json.Int.of_yojson i with
       | Ok i -> i
       | Error s -> raise (Invalid_sources s)
       end in
  let log_src = Logs.Src.create log_name in
  Option.iter (fun x -> Logs.Src.set_level log_src @@ Some x) b.logs;
  let (module Logs : Logs.LOG) = Logs.src_log log_src in
  let module SM =
    Board_protocol.Make(Logs)
      (struct let source_id = source_id end) in
  let storage = Config_storage.create base
                  ["board"; (string_of_int b.control)] in
  let events, api, step =
    SM.create send (fun x -> convert_streams x b) storage step in
  let handlers = Board_api.handlers b.control api events in
  let state = object
      method finalize () = ()
    end in
  { handlers
  ; control = b.control
  ; streams_signal = events.streams
  ; step
  ; connection = events.state
  ; ports_sync =
      List.fold_left (fun acc p ->
          (match p.port with
           | 0 -> React.S.map (function [] -> false | _ -> true)
                    events.available_streams
           | x -> invalid_port log_name x)
          |> fun x -> Ports.add p.port x acc)
        Ports.empty b.ports
  ; ports_active =
      List.fold_left (fun acc p ->
          (match p.port with
           | 0 -> React.S.const true
           | x -> invalid_port log_name x)
          |> fun x -> Ports.add p.port x acc)
        Ports.empty b.ports
  ; stream_handler = None
  ; state = (state :> < finalize : unit -> unit >)
  ; templates = None
  }
