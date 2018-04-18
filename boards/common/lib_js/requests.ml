open Containers
open Api_js.Requests
open Lwt.Infix

(* TODO fix types *)
   
let post_port control port listening =
  post_result_unit (Printf.sprintf "/api/board/%d/port/%d/%s" control port (if listening then "set" else "unset"))

let get_state control =
  get_result (Printf.sprintf "/api/board/%d/state" control)
  >|= Result.(map Common.Topology.state_of_yojson)

let get_state_ws control =
  get_socket (Printf.sprintf "api/board/%d/state_ws" control) Common.Topology.state_of_yojson
