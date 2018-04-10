open Containers
open Components
open Lwt_result.Infix

type events =
  { config   : Board_types.config React.event
  ; measures : Board_types.measure_response React.event
  }

type listener =
  { config  : Board_types.config
  ; events  : events
  ; state   : Common.Topology.state React.signal
  ; sockets : WebSockets.webSocket Js.t list
  }

let listen control : (listener,string) Lwt_result.t =
  Requests.get_config control
  >>= (fun cfg ->
    Requests.get_state control
    >>= (fun state ->
         let e_state, state_ws       = Requests.get_state_ws control in
         let e_measures, measures_ws = Requests.get_measures_ws control in
         let e_config, config_ws     = Requests.get_config_ws control in
         let events   = { config = e_config; measures = e_measures } in
         let listener = { config  = cfg
                        ; events
                        ; state   = React.S.hold ~eq:Common.Topology.equal_state state e_state
                        ; sockets = [ state_ws; measures_ws; config_ws]
                        }
         in
         Lwt_result.return listener))

let unlisten (x:(listener,string) Lwt_result.t) =
  x >>= (fun l -> List.iter (fun x -> x##close) l.sockets; Lwt_result.return ())
