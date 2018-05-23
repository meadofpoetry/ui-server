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

let create (b:topo_board) incoming_streams convert_streams send db base step =
  let storage         = Config_storage.create base ["board"; (string_of_int b.control)] in
  let s_state, spush  = React.S.create `No_response in
  let events,api,step = create_sm send storage spush step (fun x -> convert_streams x b) in
  let s_inp           = React.S.hold ~eq:equal_input storage#get.input events.input in
  (* FIXME incoming streams should be modified to include streams that are detected by the board itself *)
  let handlers        = Board_api.handlers b.control api events s_state s_inp incoming_streams in
  let state           = (object
                           method finalize () = ()
                         end) in
  { handlers       = handlers
  ; control        = b.control
  ; streams_signal = events.streams
  ; step           = step
  ; connection     = s_state
  ; ports_active   = (List.fold_left (fun acc p ->
                          (match p.port with
                           | 0 -> React.S.map (function SPI -> true | _ -> false) s_inp
                           | 1 -> React.S.map (function ASI -> true | _ -> false) s_inp
                           | x -> raise (Invalid_port ("Board_qos_niit: invalid port " ^ (string_of_int x))))
                          |> fun x -> Ports.add p.port x acc)
                                     Ports.empty b.ports)
  ; settings_page  = ("QOS", React.S.const (Tyxml.Html.div []))
  ; widgets_page   = [("QOS", React.S.const (Tyxml.Html.div []))]
  ; stream_handler = None
  ; state          = (state :> < finalize : unit -> unit >)
  }
