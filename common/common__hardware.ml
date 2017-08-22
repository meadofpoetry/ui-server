type typ = TS2IP
         | TSOIP
         | RF
         | QOS

let typ_of_yojson = function
  | `String "TS2IP" -> Ok TS2IP
  | `String "TSOIP" -> Ok TSOIP
  | `String "RF"    -> Ok RF
  | `String "QOS"   -> Ok QOS
  | _ as e  -> Error ("typ_of_yojson: unknown value " ^ (Yojson.Safe.to_string e))

let typ_to_yojson = function
  | TS2IP -> `String "TS2IP"
  | TSOIP -> `String "TSOIP"
  | RF    -> `String "RF"
  | QOS   -> `String "QOS"
           
type output = RF
            | TSOIP

let output_of_yojson = function
  | `String "RF"    -> Ok RF
  | `String "TSOIP" -> Ok TSOIP
  | _ as e -> Error ("output_of_yojson: unknown value " ^ (Yojson.Safe.to_string e))

let output_to_yojson = function
  | RF    -> `String "RF"
  | TSOIP -> `String "TSOIP"

type version = int [@@deriving yojson]

type id = int [@@deriving yojson]


type topology = topo_entry list [@@deriving yojson]

and topo_entry = Output of output
               | Board  of topo_board

and topo_board = { id      : id
                 ; typ     : typ
                 ; version : version
                 ; control : int
                 ; ports   : topo_port list
                 }

and topo_port = { port  : int
                ; child : topo_entry
                }
