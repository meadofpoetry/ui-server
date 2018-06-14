open Containers
open Api_js.Requests.Json_request
open Lwt.Infix

let post_port control port listening =
  let path = Printf.sprintf "/api/board/%d/port/%d/%s" control port (if listening then "set" else "unset") in
  post_result ~path (fun _ -> Ok ()) ()

let get_state control =
  let path = Printf.sprintf "/api/board/%d/state" control in
  get_result ~path Common.Topology.state_of_yojson ()

let get_state_ws control =
  let path = Printf.sprintf "api/board/%d/state_ws" control in
  WS.get ~path Common.Topology.state_of_yojson ()
