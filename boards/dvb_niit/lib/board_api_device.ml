open Containers
open Common
open Board_types
open Board_protocol
open Board_api_common
open Api.Interaction
open Api.Redirect

(** API
    POST /device/reset

    GET  /device/info
    GET  /device/config
    GET  /device/state
 *)

type events = device_events

module WS = struct

  let state sock_data (events:events) body () =
    sock_handler sock_data (React.S.changes events.state) Topology.state_to_yojson body

end

module HTTP = struct

end
