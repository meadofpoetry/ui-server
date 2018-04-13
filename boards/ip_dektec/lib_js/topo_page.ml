open Containers
open Components
open Settings

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

let make (board:Common.Topology.topo_board) : Ui_templates.Types.settings_section_lwt =
  let open Lwt_result.Infix in
  Listener.listen board.control
  >>= (fun (l,state) ->
    let nw_box,s_nw,submit_nw =
      make_nw_settings ~init:l.config.nw
                       ~event:(React.E.map (fun (cfg:Board_types.config) -> cfg.nw) l.events.config)
                       ~state:l.state
                       board.control
                       ()
    in
    let nw_apply = Ui_templates.Buttons.create_apply s_nw submit_nw in
    let nw_w = new Box.t
                   ~vertical:true
                   ~widgets:[ nw_box#widget
                            ; (new Card.Actions.Buttons.t ~widgets:[nw_apply] ())#widget
                            ]
                   ()
    in
    let ip_box,s_ip,submit_ip =
      make_ip_settings ~init:l.config.ip
                       ~event:(React.E.map (fun (cfg:Board_types.config) -> cfg.ip) l.events.config)
                       ~state:l.state
                       board.control
                       ()
    in
    let ip_apply = Ui_templates.Buttons.create_apply s_ip submit_ip in
    let ip_w = new Box.t
                   ~vertical:true
                   ~widgets:[ ip_box#widget
                            ; (new Card.Actions.Buttons.t ~widgets:[ip_apply] ())#widget
                            ]
                   ()
    in
    let tabs = Ui_templates.Tabs.create_simple_tabs [ `Text "Сеть", nw_w#widget
                                                    ; `Text "Приём IP", ip_w#widget ]
    in
    Lwt_result.return (tabs#widget,fun () -> Listener.unlisten state))
