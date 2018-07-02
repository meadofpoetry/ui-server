open Containers
open Common.Topology
open Boards.Board
open Board_types

module Data = struct
  type t = Board_types.config
  let default = Board_types.config_default
  let dump = Board_types.config_to_string
  let restore = Board_types.config_of_string
end

module Config_storage = Storage.Options.Make (Data)

(* module Board_model : sig
 *   type _ req =
 *     | Store_status : Board_types.status -> unit req
 *   include (Storage.Database.MODEL with type 'a req := 'a req)
 * end = Db
 * 
 * module Database = Storage.Database.Make(Board_model) *)

type 'a request = 'a Board_protocol.request

let get_active_ports (ports:topo_port list) =
  List.fold_left (fun acc (p : topo_port) ->
      (match p.port with
       | 0 -> React.S.const true
       | x -> raise (Invalid_port ("Board_ip_dektec: invalid_port " ^ (string_of_int x))))
      |> fun x -> Ports.add p.port x acc)
                 Ports.empty ports

let create (({control;ports;_} as b):topo_board) _ convert_streams send db_conf base step =
  let storage          = Config_storage.create base ["board"; (string_of_int control)] in
  let events,api,step  = Board_protocol.SM.create control send storage step in
  let handlers = Board_api.handlers control api events in
  (* let db       = Result.get_exn @@ Database.create db_conf in
   * let _s       = Lwt_react.E.map_p (fun s -> Database.request db (Board_model.Store_status s))
   *                @@ React.E.changes events.status
   * in *)
  let state = object
      (* method s = _s;
       * method db = db; *)
      method finalize () = ()
    end
  in
  { handlers
  ; control
  ; streams_signal = convert_streams events.streams b
  ; step
  ; connection     = events.state
  ; ports_active   = get_active_ports ports
  ; settings_page  = ("IP", React.S.const (Tyxml.Html.div []))
  ; widgets_page   = [("IP", React.S.const (Tyxml.Html.div []))]
  ; stream_handler = None
  ; state          = (state :> < finalize : unit -> unit >)
  }
