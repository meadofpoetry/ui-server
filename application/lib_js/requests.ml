open Containers
open Common.Topology
open Api_js.Requests
open Lwt.Infix

open Application_types

let get_topology () =
  get_js "api/topology/topology"
  >|= Result.(flat_map of_yojson)

let get_topology_socket () =
  get_socket "api/topology/topology_sock" of_yojson

let get_stream_table () =
  get_js "api/topology/stream_table"
  >|= Result.(flat_map stream_table_of_yojson)

let get_stream_table_socket () =
  get_socket "api/topology/stream_table_sock" stream_table_of_yojson

let post_stream_settings settings =
  post_js_error "api/topology/stream_settings" set_error_of_yojson (stream_setting_to_yojson settings)
