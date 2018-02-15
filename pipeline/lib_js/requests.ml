open Containers
open Api_js.Requests
open Lwt.Infix
   
let get_structure () =
  get_js "api/pipeline/structure"
  >|= Result.(flat_map Structure.t_list_of_yojson)

let post_structure s =
  Structure.t_list_to_yojson s
  |> post_js_ok "api/pipeline/structure"

let get_structure_socket () =
  get_socket "api/pipeline/structure_sock" Structure.t_list_of_yojson

let get_settings () =
  get_js "api/pipeline/settings"
  >|= Result.(flat_map Settings.of_yojson)

let post_settings s =
  Settings.to_yojson s
  |> post_js_ok "api/pipeline/settings"

let get_settings_socket () =
  get_socket "api/pipeline/settings_sock" Settings.of_yojson

let get_wm () =
  get_js "api/pipeline/wm"
  >|= Result.(flat_map Wm.of_yojson)

let post_wm wm =
  Wm.to_yojson wm
  |> post_js_ok "api/pipeline/wm"

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
