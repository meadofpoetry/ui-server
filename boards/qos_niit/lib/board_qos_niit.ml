open Common.Topology
open Api.Interaction
open Meta_board
open Board_types

module Api_handler = Api.Handler.Make (Common.User)

open Lwt.Infix

module Data = struct

  type t      = Board_types.config
  let default = Board_types.config_default
  let dump    = Board_types.config_to_string
  let restore = Board_types.config_of_string

end

module Config_storage = Storage.Options.Make (Data)

type 'a request = 'a Board_protocol.request

let create (b:topo_board) _ convert_streams send db base step =
  let conv            = fun x -> convert_streams x b in
  let storage         = Config_storage.create base ["board"; (string_of_int b.control)] in
  let events,api,step = Board_protocol.SM.create send storage step conv in
  let handlers        = Board_api.handlers b.control api events in
  let state           = (object method finalize () = () end) in
  { handlers       = handlers
  ; control        = b.control
  ; streams_signal = events.streams
  ; step           = step
  ; connection     = events.state
  ; ports_active   = List.fold_left (fun acc p ->
                         (match p.port with
                          | 0 -> React.S.map (function SPI -> true | _ -> false) events.input
                          | 1 -> React.S.map (function ASI -> true | _ -> false) events.input
                          | x -> raise (Invalid_port ("Board_qos_niit: invalid port " ^ (string_of_int x))))
                         |> fun x -> Ports.add p.port x acc) Ports.empty b.ports
  ; settings_page  = ("QOS", React.S.const (Tyxml.Html.div []))
  ; widgets_page   = [("QOS", React.S.const (Tyxml.Html.div []))]
  ; stream_handler = None
  ; state          = (state :> < finalize : unit -> unit >)
  }
