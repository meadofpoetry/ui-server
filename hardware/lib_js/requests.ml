open Common.Topology
open Api_js.Requests
open Lwt.Infix
   
let get_topology_socket () =
  get_socket "api/hardware" topology_of_yojson
