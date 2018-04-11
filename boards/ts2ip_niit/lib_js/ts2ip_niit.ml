open Containers
open Board_types
open Lwt_result.Infix

type events =
  { config : config_response React.event
  ; status : status React.event
  }

type listener =
  { config  : config_response
  ; events  : events
  ; state   : Common.Topology.state React.signal
  ; sockets : WebSockets.webSocket Js.t list
  }

type state = (listener,string) Lwt_result.t

let listen control : state =
  Requests.get_config control
  >>= (fun cfg ->
    Requests.get_state control
    >>= (fun state ->
         let e_config,config_ws = Requests.get_config_ws control in
         let e_status,status_ws = Requests.get_status_ws control in
         let e_state,state_ws   = Requests.get_state_ws control in
         let events = { config = e_config; status = e_status } in
         let listener = { config  = cfg
                        ; events
                        ; state   = React.S.hold ~eq:Common.Topology.equal_state state e_state
                        ; sockets = [ state_ws; status_ws; config_ws ]
                        }
         in
         Lwt_result.return listener))

let unlisten (x:state) =
  x >>= (fun l -> List.iter (fun x -> x##close) l.sockets; Lwt_result.return ())
