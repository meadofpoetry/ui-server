open Containers
open Common.Topology
open Api_js.Requests.Json_request
open Lwt.Infix

open Application_types

let get_topology () =
  get_result of_yojson "api/topology/topology"

let get_topology_socket () =
  WS.get "api/topology/topology_sock" of_yojson

let get_stream_table () =
  get_result stream_table_of_yojson  "api/topology/stream_table"

let get_stream_table_socket () =
  WS.get "api/topology/stream_table_sock" stream_table_of_yojson

let post_stream_settings settings =
  post_result ~contents:(stream_setting_to_yojson settings)
              ~from_err:set_error_of_yojson
              (fun _ -> Ok ())
              "api/topology/stream_settings"
