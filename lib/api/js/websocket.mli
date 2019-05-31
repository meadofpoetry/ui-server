open Js_of_ocaml
open Js_of_ocaml.WebSockets

type 'a t

include module type of Websocket_intf

val close_socket : 'a t -> unit

module type BODY = sig
  include Api.BODY
  val of_event : webSocket messageEvent Js.t -> (t, string) result
  val of_msg : Wamp.Element.t -> t
  val to_msg : t -> Wamp.Element.t
end

module Make (Body : BODY) : WS with type body := Body.t

module Json_body : BODY with type t = Yojson.Safe.json

module JSON : WS with type body := Yojson.Safe.json
                  and type t = Yojson.Safe.json t
