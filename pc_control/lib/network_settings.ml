open Containers

let domain = "network"
           
type t    = { intern : iface
            ; extern : iface option
            }
and iface = { interface   : string
            ; mac_address : bytes option
            ; address     : Network_config.v4
            ; mask        : int32
            ; gateway     : Network_config.v4
            } [@@deriving yojson]

let default = { intern = { interface   = "eno1"
                         ; mac_address = None
                         ; address     = Ipaddr.V4.of_string_exn "192.168.1.100"
                         ; mask        = 24l
                         ; gateway     = Ipaddr.V4.of_string_exn "192.168.1.1"
                         }
              ; extern = None
              }

let apply (c : Network_config.t) (s : iface) : Network_config.t option =
  if not (Option.map_or ~default:true (fun x -> Bytes.equal x c.ethernet.mac_address) s.mac_address)
     || not (Network_config.equal_v4 c.ipv4.address s.address)
     || not (Int32.equal c.ipv4.mask s.mask)
     || not (Network_config.equal_v4 c.ipv4.gateway s.gateway)
  then 
    let ethernet = match s.mac_address with
      | None -> c.ethernet
      | Some mac_address -> { mac_address }
    in
    let ipv4 = { c.ipv4 with
                 address = s.address
               ; mask    = s.mask
               ; gateway = s.gateway
               }
    in Some { c with ipv4; ethernet }
  else None
             
