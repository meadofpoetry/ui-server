open Containers
open Components
open Settings
open Board_types
open Lwt_result.Infix

module type Listened = sig
  type config
  type status
  type config_listener
  type status_listener

  val get_config    : int -> (config,'a Api_js.Requests.err) Lwt_result.t
  val listen_config : int -> config React.event * config_listener
  val listen_status : int -> status React.event * status_listener
  val unlisten_config : config_listener -> unit
  val unlisten_status : status_listener -> unit
end

module type Listener = sig
  type config
  type status
  type events =
    { config : config React.event
    ; status : status React.event
    }
  type board_info =
    { config  : config
    ; events  : events
    ; state   : Common.Topology.state React.signal
    }

  type state
  type listener = (board_info * state,string) Lwt_result.t

  val listen   : int   -> listener
  val unlisten : state -> unit
end

module Make(M:Listened) : (Listener with type config := M.config and type status := M.status) = struct
  type config = M.config
  type status = M.status
  type events =
    { config : config React.event
    ; status : status React.event
    }

  type board_info =
    { config  : config
    ; events  : events
    ; state   : Common.Topology.state React.signal
    }

  type state = WebSockets.webSocket Js.t * M.config_listener * M.status_listener
  type listener = (board_info * state,string) Lwt_result.t

  let listen control : listener =
    let (>|=) = Lwt.Infix.(>|=) in
    M.get_config control
    >>= (fun cfg ->
      Requests.get_state control
      >>= (fun state ->
           let e_config,config_ws = M.listen_config control in
           let e_status,status_ws = M.listen_status control in
           let e_state,state_ws   = Requests.get_state_ws control in
           let events = { config = e_config; status = e_status } in
           let info   = { config  = cfg
                        ; events
                        ; state   = React.S.hold ~eq:Common.Topology.equal_state state e_state
                        }
           in
           let state : state = state_ws,config_ws,status_ws in
           Lwt_result.return (info,state)))
    >|= (function
         | Ok x    -> Ok x
         | Error e -> Error (Api_js.Requests.err_to_string e))

  let unlisten (x:state) =
    let (state,config,status) = x in
    state##close; M.unlisten_config config; M.unlisten_status status

end

module Listener = Make(struct
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


let make (board:Common.Topology.topo_board) : (#Widget.widget,string) Lwt_result.t =
  Listener.listen board.control
  >>= (fun (t,state) ->
    let nw     = make_nw board t in
    let ip     = make_ip board t in
    let tabs   = Ui_templates.Tabs.create_simple_tabs [ `Text "Сеть", nw
                                                      ; `Text "Приём TSoIP", ip
                                                      ]
    in
    tabs#set_on_destroy @@ Some (fun () -> Listener.unlisten state);
    Lwt_result.return tabs)
