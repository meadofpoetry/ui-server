open Board_types
open Api_js.Requests
open Lwt.Infix

let post_reset control () =
  post_ok (Printf.sprintf "api/board/%d/reset" control)

let post_mode control mode =
  mode_to_yojson mode
  |> post_js_ok (Printf.sprintf "api/board/%d/mode" control)

let post_jitter_mode control mode =
  jitter_mode_to_yojson mode
  |> post_js_ok (Printf.sprintf "api/board/%d/jitter_mode" control)

let get_t2mi_seq control seconds =
  get_js (Printf.sprintf "api/board/%d/t2mi_seq/%d" control seconds)
  >|= CCResult.(flat_map t2mi_seq_of_yojson)
