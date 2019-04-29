open Js_of_ocaml
open Js_of_ocaml.WebSockets

let ( % ) f g x = f (g x)

module type BODY = sig
  include Api.BODY
end

module Make(Body : BODY) : sig

  open Netlib

  val create : ?secure:bool
    -> ?host:string
    -> ?port:int
    -> ?on_message:((Body.t, string) result -> unit)
    -> path:('a, 'b) Uri.Path.Format.t
    -> query:('b, ((webSocket Js.t, string) result)) Uri.Query.format
    -> 'a

end = struct

  open Netlib

  let make_uri ?(secure = false) ?host ?port ~f ~path ~query =
    let scheme = if secure then "wss" else "ws" in
    Uri.kconstruct ~scheme ?host ?port ~path ~query
      ~f:(f % Uri.pct_decode % Uri.to_string)

  let create ?secure ?host ?port ?on_message ~path ~query =
    let f uri : (webSocket Js.t, string) result =
      let socket = new%js webSocket (Js.string uri) in
      let handler = fun (msg : webSocket messageEvent Js.t) ->
        Js._true in
      socket##.onmessage := Dom.handler handler;
      Ok socket in
    make_uri ?secure ?host ?port ~path ~query ~f

end
