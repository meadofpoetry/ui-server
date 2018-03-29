open Containers
open Common.Topology
open Api_js.Requests
open Lwt.Infix

let get_topology () =
  get_js "api/topology/topology"
  >|= Result.(flat_map of_yojson)

let get_topology_socket () =
  get_socket "api/topology/topology_sock" of_yojson
