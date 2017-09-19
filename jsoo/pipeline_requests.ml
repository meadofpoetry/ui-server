open Jsrequest
open Lwt.Infix

let get_streams () =
  get_js "api/streams"
  >|= CCResult.(flat_map Common.Streams.of_yojson)

let post_streams s =
  Common.Streams.to_yojson s
  |> post_js_ok "api/streams"

let get_streams_socket () =
  get_socket "api/streams" Common.Streams.of_yojson

let get_settings () =
  get_js "api/settings"
  >|= CCResult.(flat_map Common.Settings.of_yojson)

let post_settings s =
  Common.Settings.to_yojson s
  |> post_js_ok "api/settings"

let get_settings_socket () =
  get_socket "api/settings" Common.Settings.of_yojson
