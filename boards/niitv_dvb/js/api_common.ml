module Api_http = Api_js.Http.Make(Application_types.Body)

module Api_websocket = Api_js.Websocket.Make(
  struct
    open Js_of_ocaml
    open Js_of_ocaml.WebSockets
    include Application_types.Body
    let of_event (evt : webSocket messageEvent Js.t) =
      of_string @@ Js.to_string evt##.data
  end)

let get_api_path = Application_types.Topology.get_api_path
