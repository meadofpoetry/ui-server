open Api_js.Requests
open Lwt.Infix
   
let get_structure () =
  get_js "api/pipeline/structure"
  >|= CCResult.(flat_map Structure.t_list_of_yojson)

let post_structure s =
  Structure.t_list_to_yojson s
  |> post_js_ok "api/pipeline/structure"

let get_structure_socket () =
  get_socket "api/pipeline/structure_sock" Structure.t_list_of_yojson

let get_settings () =
  get_js "api/pipeline/settings"
  >|= CCResult.(flat_map Settings.of_yojson)

let post_settings s =
  Settings.to_yojson s
  |> post_js_ok "api/pipeline/settings"

let get_settings_socket () =
  get_socket "api/pipeline/settings" Settings.of_yojson

let get_wm () =
  get_js "api/pipeline/wm"
  >|= CCResult.(flat_map Wm.of_yojson)

let post_wm wm =
  Wm.to_yojson wm
  |> post_js_ok "api/pipeline/wm"

let get_wm_socket () =
  get_socket "api/pipeline/wm_sock" Wm.of_yojson
