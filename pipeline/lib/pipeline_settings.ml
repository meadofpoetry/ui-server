type format = [ `Json | `Msgpack ]

let format_of_yojson = function `String "json" -> Ok `Json
                              | `String "msgpack" -> Ok `Msgpack
                              | _ -> Error "Wrong fmt"

let format_to_yojson = function `Json -> `String "json"
                              | `Msgpack -> `String "msgpack"

let format_to_string = function `Json -> "json"
                              | `Msgpack -> "msgpack"

type t = { bin_name  : string
         ; bin_path  : string
         ; msg_fmt   : format
         ; sock_in   : string
         ; sock_out  : string
         } [@@deriving yojson]

let default = { bin_name  = "ats3-backend"
              ; bin_path  = "/home/freyr/Documents/soft/dev/ats-analyzer/build/"
              ; msg_fmt   = `Json
              ; sock_in   = "ipc:///tmp/ats_qoe_in"
              ; sock_out  = "ipc:///tmp/ats_qoe_out"
              }

let domain = "pipeline"
