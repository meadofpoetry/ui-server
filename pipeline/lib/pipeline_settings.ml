type t = { bin_name  : string
         ; bin_path  : string
         ; sock_in   : string
         ; sock_out  : string
         } [@@deriving yojson]

let default = { bin_name  = "ats3-backend"
              ; bin_path  = "/home/freyr/Documents/soft/dev/ats-analyzer/build/"
              ; sock_in   = "ipc:///tmp/ats_qoe_in"
              ; sock_out  = "ipc:///tmp/ats_qoe_out"
              }

let domain = "pipeline"
