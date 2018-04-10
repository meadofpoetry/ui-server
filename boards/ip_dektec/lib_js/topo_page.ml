open Containers
open Components
open Settings

let make ~make_tabs (board:Common.Topology.topo_board) =
  let open Lwt_result.Infix in
  let listener = Ip_dektec.listen board.control in
  let box      = new Box.t ~vertical:true ~widgets:[] () in
  listener
  >>= (fun l ->
    let nw_box,nw_s,nw_submit =
      make_nw_settings ~init:l.config.nw
                       ~event:(React.E.map (fun (cfg:Board_types.config) -> cfg.nw) l.events.config)
                       ~state:l.state
                       board.control
                       ()
    in
    let nw_apply = new Button.t ~label:"Применить" () in
    let _   = React.S.map (function Some _ -> nw_apply#set_disabled false
                                  | None   -> nw_apply#set_disabled true) nw_s
    in
    let _   = React.E.map (fun _ -> Option.iter (fun nw -> nw_submit nw |> ignore) @@ React.S.value nw_s)
                          nw_apply#e_click
    in
    let nw_w = new Box.t
                   ~vertical:true
                   ~widgets:[ nw_box#widget
                            ; (new Card.Actions.Buttons.t ~widgets:[nw_apply] ())#widget
                            ]
                   ()
    in
    let ip_box,ip_s,ip_submit =
      make_ip_settings ~init:l.config.ip
                       ~event:(React.E.map (fun (cfg:Board_types.config) -> cfg.ip) l.events.config)
                       ~state:l.state
                       board.control
                       ()
    in
    let ip_apply = new Button.t ~label:"Применить" () in
    let _   = React.S.map (function Some _ -> ip_apply#set_disabled false
                                  | None   -> ip_apply#set_disabled true) ip_s
    in
    let _   = React.E.map (fun _ -> Option.iter (fun ip -> ip_submit ip |> ignore) @@ React.S.value ip_s)
                          ip_apply#e_click
    in
    let ip_w = new Box.t
                   ~vertical:true
                   ~widgets:[ ip_box#widget
                            ; (new Card.Actions.Buttons.t ~widgets:[ip_apply] ())#widget
                            ]
                   ()
    in
    let bar = make_tabs [ "Сеть", nw_w#widget; "Приём IP", ip_w#widget ] in
    Dom.appendChild box#root bar#root;
    Lwt_result.return ()) |> ignore;
  box#widget, (fun () -> Ip_dektec.unlisten listener |> ignore)
