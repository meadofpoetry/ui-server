open Containers
open Components

module Listener = Boards_js.Topo_listener.Make(struct
                                                type config = Board_types.config
                                                type status = Board_types.measure_response
                                                type config_listener = WebSockets.webSocket Js.t
                                                type status_listener = WebSockets.webSocket Js.t

                                                let get_config    = Requests.get_config
                                                let listen_config = Requests.get_config_ws
                                                let listen_status = Requests.get_measures_ws
                                                let unlisten_config c = c##close
                                                let unlisten_status s = s##close
                                              end)

let make ?error_prefix (board:Common.Topology.topo_board) : (#Widget.widget,string) Lwt_result.t =
  let open Lwt_result.Infix in
  let listener = Listener.listen board.control in
  listener
  >>= (fun (l,state) ->
    List.map (fun (id,init) ->
        let event   = React.E.map (fun x -> List.Assoc.get_exn ~eq:(=) id x) l.events.config in
        let w,s,set = Settings.make_module_settings ~id ~init ~event ~state:l.state board.control () in
        let a       = Ui_templates.Buttons.create_apply s set in
        let abox    = new Card.Actions.t ~widgets:[a#widget] () in
        let b       = new Box.t ~vertical:true ~widgets:[w;abox#widget] () in
        `Text (Printf.sprintf "Модуль %d" (succ id)), b#widget)
             (List.sort (fun (id1,_) (id2,_) -> compare id1 id2) l.config)
    |> Ui_templates.Tabs.create_simple_tabs
    |> fun bar -> bar#set_on_destroy @@ Some (fun () -> Listener.unlisten state);
                  Lwt_result.return bar)
