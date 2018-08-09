open Containers
open Common.Topology
open Api.Interaction
open Boards.Board
open Board_types

module Api_handler = Api.Handler.Make (Common.User)

open Lwt.Infix

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

let create (b:topo_board) _ convert_streams send db_conf base step =
  let storage = Config_storage.create base ["board"; (string_of_int b.control)] in
  let events,api,step = Board_protocol.SM.create (log_prefix b.control) send storage step in
  let handlers        = Board_api.handlers b.control api events in
  (* let db              = Result.get_exn @@ Database.create db_conf in
   * let _s              = Lwt_react.E.map_p (fun m -> Database.request db (Board_model.Store_measures m))
   *                       @@ React.E.changes events.measures in *)
  let streams = convert_streams events.streams b in
  let state = (object
                 (* method _s = _s;
                  * method db = db; *)
                 method finalize () = ()
               end) in
  { handlers
  ; control        = b.control
  ; streams_signal = streams
  ; step
  ; connection     = events.state
  ; ports_sync     =
      List.fold_left (fun acc p ->
          (match p.port with
           | 0 -> React.S.map (function [] -> false | _ -> true) streams
           | x -> invalid_port x)
          |> fun x -> Ports.add p.port x acc)
        Ports.empty b.ports
  ; ports_active   =
      List.fold_left (fun acc p ->
          (match p.port with
           | 0 -> React.S.const true
           | x -> invalid_port x)
          |> fun x -> Ports.add p.port x acc)
        Ports.empty b.ports
  ; settings_page  = ("DVB", React.S.const (Tyxml.Html.div []))
  ; widgets_page   = [("DVB", React.S.const (Tyxml.Html.div []))]
  ; stream_handler = None
  ; state          = (state :> < finalize : unit -> unit >)
  }
