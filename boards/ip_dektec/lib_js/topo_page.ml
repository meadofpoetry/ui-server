open Containers
open Components
open Settings
open Board_types
open Lwt_result.Infix

(* let make_nw (board:Common.Topology.topo_board) ({config;state;events}:Listener.board_info) =
 *   let init    = config.nw in
 *   let event   = React.E.map (fun cfg -> cfg.nw) events.config in
 *   let w,s,set = make_nw_settings ~init ~event ~state board.control () in
 *   let a       = Ui_templates.Buttons.create_apply s set in
 *   let abox    = (new Card.Actions.Buttons.t ~widgets:[a] ())#widget in
 *   let box     = (new Box.t ~vertical:true ~widgets:[w;abox] ())#widget in
 *   box
 * 
 * let make_ip (board:Common.Topology.topo_board) ({config;state;events}:Listener.board_info) =
 *   let init    = config.ip in
 *   let event   = React.E.map (fun cfg -> cfg.ip) events.config in
 *   let w,s,set = make_ip_settings ~init ~event ~state board.control () in
 *   let a       = Ui_templates.Buttons.create_apply s set in
 *   let abox    = (new Card.Actions.Buttons.t ~widgets:[a] ())#widget in
 *   let box     = (new Box.t ~vertical:true ~widgets:[w;abox] ())#widget in
 *   box *)


let make (board:Common.Topology.topo_board) : (#Widget.widget,string) Lwt_result.t =
  let nw     = new Box.t ~widgets:[] () in
  let ip     = new Box.t ~widgets:[] () in
  let tabs   = Ui_templates.Tabs.create_simple_tabs [ `Text "Сеть", nw#widget
                                                    ; `Text "Приём TSoIP", ip#widget
                                                    ]
  in
  Lwt_result.return tabs
