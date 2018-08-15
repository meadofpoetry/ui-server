open Containers
open Common.Topology
open Boards.Board
open Board_types

module Api_handler = Api.Handler.Make (Common.User)

module Data = struct
  type t      = config
  let default = config_default
  let dump    = config_to_string
  let restore = config_of_string
end

module Config_storage = Storage.Options.Make (Data)

(* module Database = Storage.Database.Make(Board_model) *)

let log_prefix control = Printf.sprintf "(Board DVB: %d) " control

let invalid_port port =
  let s = "Board_dvb_niit: invalid port " ^ (string_of_int port) in
  raise (Invalid_port s)

let create (b:topo_board) _ convert_streams send _ (* db_conf*) base step =
  let prefix = log_prefix b.control in
  let source = match b.sources with
    | None ->
       let s = prefix ^ "no source provided!" in
       raise (Invalid_sources s)
    | Some i ->
       begin match Common.Json.Int.of_yojson i with
       | Ok i -> i
       | Error s -> raise (Invalid_sources s)
       end in
  let storage = Config_storage.create base ["board"; (string_of_int b.control)] in
  let events, api, step =
    Board_protocol.SM.create source prefix send storage step in
  let handlers = Board_api.handlers b.control api events in
  let streams = convert_streams events.streams b in
  let state = object
      (* method _s = _s;
       * method db = db; *)
      method finalize () = ()
    end in
  { handlers
  ; control = b.control
  ; streams_signal = streams
  ; step
  ; connection = events.state
  ; ports_sync =
      List.fold_left (fun acc p ->
          (match p.port with
           | 0 -> React.S.map (function [] -> false | _ -> true) streams
           | x -> invalid_port x)
          |> fun x -> Ports.add p.port x acc)
        Ports.empty b.ports
  ; ports_active =
      List.fold_left (fun acc p ->
          (match p.port with
           | 0 -> React.S.const true
           | x -> invalid_port x)
          |> fun x -> Ports.add p.port x acc)
        Ports.empty b.ports
  ; stream_handler = None
  ; state = (state :> < finalize : unit -> unit >)
  }
