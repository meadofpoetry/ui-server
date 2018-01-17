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

let stream_to_packer (b:topo_board) (s:stream_setting) : packer_setting option =
  match s.stream.id, Common.Stream.t_to_topo_port b s.stream with
  | `Ts id, Some p -> Some { stream    = s.stream
                           ; stream_id = Common.Stream.id_to_int32 id
                           ; port      = p.port
                           ; self_port = 2027
                           ; dst_ip    = s.dst_ip
                           ; dst_port  = s.dst_port
                           ; enabled   = s.enabled
                           }
  | _ -> None

let create (b:topo_board) (streams:Common.Stream.t list React.signal) _ send db base step =
  let storage          = Config_storage.create base ["board"; (string_of_int b.control)] in
  let s_state, spush   = React.S.create `No_response in
  let convert          = stream_to_packer b in
  let events,api,step  = create_sm send storage spush step convert in
  let handlers         = Board_api.handlers b.control api events s_state streams in
  let s_sms,s_sms_push = React.S.create [] in
  let state        = (object end) in
  { handlers       = handlers
  ; control        = b.control
  ; streams_signal = s_sms
  ; step           = step
  ; connection     = s_state
  ; ports_active   = List.fold_left (fun acc (p:topo_port)-> Ports.add p.port (React.S.const true) acc)
                                    Ports.empty b.ports
  ; settings_page  = ("TS2IP", React.S.const (Tyxml.Html.div []))
  ; widgets_page   = [("TS2IP", React.S.const (Tyxml.Html.div []))]
  ; state          = (state :> < >)
  }
