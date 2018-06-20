open Common.Topology
open Boards.Board
open Board_types
open Containers

module Api_handler = Api.Handler.Make (Common.User)

module Data = struct

  type t      = config
  let default = config_default
  let dump    = config_to_string
  let restore = config_of_string

end

module Config_storage = Storage.Options.Make (Data)

let log_fmt control = Printf.sprintf "(Board QoS: %d) %s" control

let create (b:topo_board) _ convert_streams send db_conf base step =
  let conv            = fun x -> convert_streams x b in
  let storage         = Config_storage.create base ["board"; (string_of_int b.control)] in
  let events,api,step = Board_protocol.SM.create (log_fmt b.control) send storage step conv in
  let handlers        = Board_api.handlers b.control api events in
  let db              = Result.get_exn @@ Db.Conn.create db_conf in
  let state           = (object val db = db method finalize () = () end) in (* TODO fix finalize *)
  { handlers       = handlers
  ; control        = b.control
  ; streams_signal = events.streams.streams
  ; step           = step
  ; connection     = events.device.state
  ; ports_active   =
      List.fold_left (fun acc p ->
          (match p.port with
           | 0 -> React.S.map (function SPI -> true | _ -> false) events.device.input
           | 1 -> React.S.map (function ASI -> true | _ -> false) events.device.input
           | x -> raise (Invalid_port ("Board_qos_niit: invalid port " ^ (string_of_int x))))
          |> fun x -> Ports.add p.port x acc) Ports.empty b.ports
  ; settings_page  = ("QOS", React.S.const (Tyxml.Html.div []))
  ; widgets_page   = [("QOS", React.S.const (Tyxml.Html.div []))]
  ; stream_handler = None
  ; state          = (state :> < finalize : unit -> unit >)
  }
