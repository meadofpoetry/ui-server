open Containers
open Api_js.Requests.Json_request
open Lwt.Infix

open Qoe_errors_types

let get_structure () =
  get_result Structure.Streams.of_yojson "api/pipeline/structure"

let post_structure s =
  post_result ~contents:(Structure.Streams.to_yojson s) (fun _ -> Ok ()) "api/pipeline/structure"

let get_structure_socket () =
  WS.get "api/pipeline/structure_sock" Structure.Streams.of_yojson

let get_settings () =
  get_result Settings.of_yojson "api/pipeline/settings"

let post_settings s =
  post_result ~contents:(Settings.to_yojson s) (fun _ -> Ok ()) "api/pipeline/settings"

let get_settings_socket () =
  WS.get "api/pipeline/settings_sock" Settings.of_yojson

let get_wm () =
  get_result Wm.of_yojson "api/pipeline/wm"

let post_wm wm =
  post_result ~contents:(Wm.to_yojson wm) (fun _ -> Ok ()) "api/pipeline/wm"

let get_wm_socket () =
  WS.get "api/pipeline/wm_sock" Wm.of_yojson

  (*
let get_vdata_socket () =
  WS.get "api/pipeline/vdata_sock" Video_data.of_yojson

let get_vdata_socket_stream stream =
  let path = Printf.sprintf "api/pipeline/vdata_sock/%d" stream in
  WS.get path Video_data.of_yojson

let get_vdata_socket_channel stream channel =
  let path = Printf.sprintf "api/pipeline/vdata_sock/%d/%d" stream channel in
  WS.get path Video_data.of_yojson

let get_vdata_socket_pid stream channel pid =
  let path = Printf.sprintf "api/pipeline/vdata_sock/%d/%d/%d" stream channel pid in
  WS.get path Video_data.of_yojson
   *)
