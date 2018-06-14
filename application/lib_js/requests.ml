open Containers
open Common.Topology
open Api_js.Requests.Json_request
open Lwt.Infix

open Application_types

let get_topology () =
  let path = "api/topology/topology" in
  get_result ~path of_yojson ()

let get_topology_socket () =
  let path = "api/topology/topology_sock" in
  WS.get ~path of_yojson ()

let get_stream_table () =
  let path = "api/topology/stream_table" in
  get_result ~path stream_table_of_yojson ()

let get_stream_table_socket () =
  let path = "api/topology/stream_table_sock" in
  WS.get ~path stream_table_of_yojson ()

let post_stream_settings settings =
  let path = "api/topology/stream_settings" in
  post_result ~path ~contents:(stream_setting_to_yojson settings)
              ~from_err:set_error_of_yojson
              (fun _ -> Ok ())
              ()
