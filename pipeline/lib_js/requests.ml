open Api_js.Requests
open Lwt.Infix
   
let get_structure () =
  get_js "api/structure"
  >|= CCResult.(flat_map Structure.t_list_of_yojson)

let post_structure s =
  Structure.t_list_to_yojson s
  |> post_js_ok "api/structure"

let get_structure_socket () =
  get_socket "api/structure_sock" Structure.t_list_of_yojson

let get_settings () =
  get_js "api/settings"
  >|= CCResult.(flat_map Settings.of_yojson)

let post_settings s =
  Settings.to_yojson s
  |> post_js_ok "api/settings"

let get_settings_socket () =
  get_socket "api/settings" Settings.of_yojson
