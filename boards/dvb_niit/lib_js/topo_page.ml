open Containers
open Components
open Boards_js.Topo_components
open Boards_js.Types

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

let make (board:Common.Topology.topo_board) : topo_settings_result =
  let open Lwt_result.Infix in
  let listener = Listener.listen board.control in
  listener
  >>= (fun (l,state) ->
    let pages =
      List.map (fun (id,init) ->
          let event = React.E.map (fun x -> List.Assoc.get_exn ~eq:(=) id x) l.events.config in
          let b,s,submit = Settings.make_module_settings ~id ~init ~event ~state:l.state board.control () in
          let apply   = make_apply s submit in
          let actions = new Card.Actions.t ~widgets:[apply#widget] () in
          let w = new Box.t ~vertical:true ~widgets:[b#widget;actions#widget] () in
          Printf.sprintf "Модуль %d" (succ id), w#widget)
               (List.sort (fun (id1,_) (id2,_) -> compare id1 id2) l.config)
    in
    let tabs = make_tabs pages in
    Lwt_result.return (tabs#widget,fun () -> Listener.unlisten state))
