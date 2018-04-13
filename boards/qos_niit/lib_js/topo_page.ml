open Containers
open Components
open Board_types
open Boards_js.Topo_components
open Boards_js.Types

module Listener = Boards_js.Topo_listener.Make(struct
                                                type config = Board_types.config
                                                type status = Board_types.user_status
                                                type config_listener = WebSockets.webSocket Js.t
                                                type status_listener = WebSockets.webSocket Js.t

                                                let get_config    = Requests.get_config
                                                let listen_config = Requests.get_config_ws
                                                let listen_status = Requests.get_status_ws
                                                let unlisten_config c = c##close
                                                let unlisten_status s = s##close
                                              end)

let make (board:Common.Topology.topo_board) : topo_settings_result =
  let open Lwt_result.Infix in
  Requests.get_incoming_streams board.control
  >>= (fun streams ->
    Listener.listen board.control
    >>= (fun (l,state) ->
         let e_streams,sock = Requests.get_incoming_streams_ws board.control in
         let s_streams  = React.S.hold streams e_streams in
         let mw,ms,mset = Settings.make_t2mi_mode
                            ~init:l.config.mode.t2mi
                            ~event:(React.E.map (fun (x:config) -> x.mode.t2mi) l.events.config)
                            ~streams:s_streams
                            ~state:l.state
                            board.control
                            ()
         in
         let jw,js,jset = Settings.make_jitter_mode
                            ~init:l.config.jitter_mode
                            ~event:(React.E.map (fun (x:config) -> x.jitter_mode) l.events.config)
                            ~state:l.state
                            board.control
                            ()
         in
         let tabs = make_tabs ["T2-MI", mw; "Джиттер", jw ] in
         Lwt_result.return (tabs#widget,fun () -> sock##close; Listener.unlisten state)))
