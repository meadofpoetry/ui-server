open Application_types

let map_err = function
  | Error e -> Error (`Conv_error e)
  | Ok _ as x -> x

let ignore_env = fun _ x -> Lwt.return x

let ignore_env_bind f = fun _ -> function
  | Error _ as e -> Lwt.return e
  | Ok x -> f x

module Body = struct
  open Js_of_ocaml
  open Js_of_ocaml.WebSockets
  include Application_types.Body
  let of_event (evt : webSocket messageEvent Js.t) =
    of_string @@ Js.to_string evt##.data
end

module Api_http = Api_js.Http.Make(Body)

module Api_websocket = Api_js.Websocket.Make(Body)

let stream_assoc_list_of_yojson _of =
  Util_json.(List.of_yojson (Pair.of_yojson Stream.ID.of_yojson _of))
