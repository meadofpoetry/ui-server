open Containers
open Api_js.Requests
open Lwt.Infix
open Qoe_errors

(* TODO fix types *)
let get_structure () =
  get_result Structure.Streams.of_yojson (fun _ -> Ok ("unknown error")) "api/pipeline/structure"

let post_structure s =
  post_result_unit (fun _ -> Ok ("unknown error")) "api/pipeline/structure" ~contents:(Structure.Streams.to_yojson s)

let get_structure_socket () =
  get_socket "api/pipeline/structure_sock" Structure.Streams.of_yojson

let get_settings () =
  get_result Settings.of_yojson (fun _ -> Ok ("unknown error")) "api/pipeline/settings"

let post_settings s =
  post_result_unit (fun _ -> Ok ("unknown error")) "api/pipeline/settings" ~contents:(Settings.to_yojson s)

let get_settings_socket () =
  get_socket "api/pipeline/settings_sock" Settings.of_yojson

let get_wm () =
  get_result Wm.of_yojson (fun _ -> Ok ("unknown error")) "api/pipeline/wm"

let post_wm wm =
  post_result_unit (fun _ -> Ok ("unknown error")) "api/pipeline/wm" ~contents:(Wm.to_yojson wm)

let get_wm_socket () =
  get_socket "api/pipeline/wm_sock" Wm.of_yojson

let get_vdata_socket () =
  get_socket "api/pipeline/vdata_sock" Video_data.of_yojson

let get_vdata_socket_stream stream =
  let path = Printf.sprintf "api/pipeline/vdata_sock/%d" stream in
  get_socket path Video_data.of_yojson

let get_vdata_socket_channel stream channel =
  let path = Printf.sprintf "api/pipeline/vdata_sock/%d/%d" stream channel in
  get_socket path Video_data.of_yojson

let get_vdata_socket_pid stream channel pid =
  let path = Printf.sprintf "api/pipeline/vdata_sock/%d/%d/%d" stream channel pid in
  get_socket path Video_data.of_yojson
