open Containers
open Common.Topology
open Api_js.Requests
open Lwt.Infix

let get_topology () =
  get_js "api/hardware/topology"
  >|= Result.(flat_map topology_of_yojson)

let get_topology_socket () =
  get_socket "api/hardware/topology_sock" topology_of_yojson
