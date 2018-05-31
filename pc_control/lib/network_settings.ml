open Containers

let domain = "network"
           
type t    = { intern : iface
            ; extern : iface option
            }
and iface = { interface   : string
            ; mac_address : Network_config.Macaddr.t option
            ; address     : Network_config.address
            ; routes      : Network_config.routes
            } [@@deriving yojson]

let default = { intern = { interface   = "eno1"
                         ; mac_address = None
                         ; address     = (Ipaddr.V4.of_string_exn "192.168.1.100", 24l)
                         ; routes      = { static  = []
                                         ; gateway = Ipaddr.V4.of_string "192.168.1.1"
                                         }
                         }
              ; extern = None
              }

let apply (c : Network_config.t) (s : iface) : Network_config.t option =
  if not (Option.map_or ~default:true (fun x -> Network_config.Macaddr.equal x c.ethernet.mac_address) s.mac_address)
     || not (Network_config.equal_address c.ipv4.address s.address)
     || not (Network_config.equal_routes c.ipv4.routes s.routes)
  then 
    let ethernet = match s.mac_address with
      | None -> c.ethernet
      | Some mac_address -> { mac_address }
    in
    let ipv4 = { c.ipv4 with
                 address = s.address
               ; routes  = s.routes
               }
    in Some { c with ipv4; ethernet }
  else None
             
