type adapter = | DVB
               | TS
               | IP

let adapter_of_yojson = function
  | `String "DVB" -> Ok DVB
  | `String "TS"  -> Ok TS
  | `String "IP"  -> Ok IP
  | _ as e        -> Error ("adapter_of_yojson: unknown value " ^ (Yojson.Safe.to_string e))

let adapter_to_yojson = function
  | DVB   -> `String "DVB"
  | TS    -> `String "TS"
  | IP    -> `String "IP"

type converter = IP

let converter_of_yojson = function
  | `String "IP" -> Ok IP
  | _ as e       -> Error ("converter_of_yojson: unknown value " ^ (Yojson.Safe.to_string e))

let converter_to_yojson = function
  | IP -> `String "IP"

type typ = Adapter of adapter
         | Converter of converter [@@deriving yojson]

type input = RF
           | TSOIP
           | ASI

let input_of_yojson = function
  | `String "RF"    -> Ok RF
  | `String "TSOIP" -> Ok TSOIP
  | `String "ASI"   -> Ok ASI
  | _ as e -> Error ("output_of_yojson: unknown value " ^ (Yojson.Safe.to_string e))

let input_to_yojson = function
  | RF    -> `String "RF"
  | TSOIP -> `String "TSOIP"
  | ASI   -> `String "ASI"

type version = int [@@deriving yojson]

type id = int [@@deriving yojson]

type topology = topo_entry list [@@deriving yojson]

and topo_entry = Input of input
               | Board  of topo_board

and topo_board = { typ          : typ
                 ; model        : string
                 ; manufacturer : string
                 ; version      : version
                 ; control      : int
                 ; active       : bool
                 ; ports        : topo_port list
                 }

and topo_port = { port      : int
                ; listening : bool
                ; child     : topo_entry
                }

let get_api_path = string_of_int
