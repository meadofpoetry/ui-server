(*
type graph_state = Null
                 | Pause
                 | Play
                 | Stop

let graph_state_of_yojson = function
    | `String "null"  -> Ok Null
    | `String "pause" -> Ok Pause
    | `String "play"  -> Ok Play
    | `String "stop"  -> Ok Stop
    | err -> Error ("graph_state_of_yojson: wrong data" ^ (Yojson.Safe.to_string err))

let graph_state_to_yojson = function
  | Null  -> `String "null"
  | Pause -> `String "pause"
  | Play  -> `String "play"
  | Stop  -> `String "stop"

type t ={ state : graph_state
        } [@@deriving yojson]
 *)

module Applied_structures = struct
  include Structure.Structures
  let name = "graph"
end
