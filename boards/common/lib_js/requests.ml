open Containers
open Api_js.Requests.Json_request
open Lwt.Infix

let post_port control port listening =
  let path = Printf.sprintf "/api/board/%d/port/%d/%s" control port
                            (if listening then "set" else "unset") in
  post_result (fun _ -> Ok ()) path

module Board_real_time = struct

  let get_state control =
    get_result Common.Topology.state_of_yojson (Printf.sprintf "/api/board/%d/state" control)

  let get_state_ws control =
    WS.get (Printf.sprintf "api/board/%d/state_ws" control) Common.Topology.state_of_yojson

end
