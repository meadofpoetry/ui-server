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

let create_sm = Board_protocol.SM.create

let create (b:topo_board) convert_streams send db base step =
  let storage          = Config_storage.create base ["board"; (string_of_int b.control)] in
  let s_state, spush   = React.S.create `No_response in
  let events,api,step  = create_sm send storage spush step in
  let handlers         = Board_api.handlers b.control api events in
  let s_streams        = React.S.const [] in
  let sms = convert_streams s_streams b in
  let _e = React.E.map (fun s ->
               `List (List.map Common.Stream.to_yojson s)
               |> Yojson.Safe.pretty_to_string
               |> Lwt_io.printf "QOS sms: %s\n"
               |> ignore;) @@ React.S.changes sms in
  let state           = (object
                           method s_streams = s_streams;
                           method _e = _e;
                         end) in
  { handlers       = handlers
  ; control        = b.control
  ; streams_signal = sms
  ; step           = step
  ; connection     = s_state
  ; ports_active   = List.fold_left (fun acc p -> Ports.add p.port (React.S.const true) acc)
                                    Ports.empty b.ports
  ; settings_page  = ("TS2IP", React.S.const (Tyxml.Html.div []))
  ; widgets_page   = [("TS2IP", React.S.const (Tyxml.Html.div []))]
  ; state          = (state :> < >)
  }
