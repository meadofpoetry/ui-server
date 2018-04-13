open Containers
open Components
open Settings
open Board_types
open Lwt_result.Infix

module Listener = Boards_js.Topo_listener.Make(struct
                                                type config = Board_types.config
                                                type status = Board_types.board_status
                                                type config_listener = WebSockets.webSocket Js.t
                                                type status_listener = WebSockets.webSocket Js.t

                                                let get_config    = Requests.get_config
                                                let listen_config = Requests.get_config_ws
                                                let listen_status = Requests.get_status_ws
                                                let unlisten_config c = c##close
                                                let unlisten_status s = s##close
                    end)

let make_nw (board:Common.Topology.topo_board) ({config;state;events}:Listener.board_info) =
  let init    = config.nw in
  let event   = React.E.map (fun cfg -> cfg.nw) events.config in
  let w,s,set = make_nw_settings ~init ~event ~state board.control () in
  let a       = Ui_templates.Buttons.create_apply s set in
  let abox    = (new Card.Actions.Buttons.t ~widgets:[a] ())#widget in
  let box     = (new Box.t ~vertical:true ~widgets:[w;abox] ())#widget in
  box

let make_ip (board:Common.Topology.topo_board) ({config;state;events}:Listener.board_info) =
  let init    = config.ip in
  let event   = React.E.map (fun cfg -> cfg.ip) events.config in
  let w,s,set = make_ip_settings ~init ~event ~state board.control () in
  let a       = Ui_templates.Buttons.create_apply s set in
  let abox    = (new Card.Actions.Buttons.t ~widgets:[a] ())#widget in
  let box     = (new Box.t ~vertical:true ~widgets:[w;abox] ())#widget in
  box


let make (board:Common.Topology.topo_board) : Ui_templates.Types.settings_section_lwt =
  Listener.listen board.control
  >>= (fun (t,state) ->
    let nw     = make_nw board t in
    let ip     = make_ip board t in
    let tabs   = Ui_templates.Tabs.create_simple_tabs [ `Text "Сеть", nw
                                                      ; `Text "Приём TSoIP", ip
                                                      ]
    in
    Lwt_result.return (tabs,fun () -> Listener.unlisten state))
