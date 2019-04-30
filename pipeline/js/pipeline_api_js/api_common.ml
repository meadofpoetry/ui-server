open Application_types

let ( % ) f g x = f (g x)

let map_ok f = function Ok x -> Ok (f x) | Error e -> Error e

module Api_http = Api_js.Http.Make(Body)

module Api_websocket = Api_js.Websocket.Make(
  struct
    open Js_of_ocaml
    open Js_of_ocaml.WebSockets
    include Body
    let of_event evt = of_string @@ Js.to_string evt##.data
  end)
