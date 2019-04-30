open Js_of_ocaml
open Js_of_ocaml.WebSockets

let ( % ) f g x = f (g x)

let return_ok x = Ok x

module type BODY = sig
  include Api.BODY
  val of_event : webSocket messageEvent Js.t -> (t, [ `Conv_error of string]) result
end

module Make(Body : BODY) : sig

  open Netlib

  type t = webSocket Js.t

  val create : ?secure:bool
    -> ?host:string
    -> ?port:int
    -> ?on_error:(webSocket Dom.event Js.t -> unit)
    -> path:('a, 'b) Uri.Path.Format.t
    -> query:('b, unit -> ((t, string) Lwt_result.t)) Uri.Query.format
    -> 'a

  val subscribe : ((Body.t, string) result -> unit) -> t -> unit

end = struct

  open Netlib

  type t = webSocket Js.t

  let make_uri ?(secure = false) ?host ?port ~f ~path ~query =
    let scheme = if secure then "wss" else "ws" in
    Uri.kconstruct ~scheme ?host ?port ~path ~query
      ~f:(f % Uri.pct_decode % Uri.to_string)

  let create ?secure ?host ?port ?on_error ~path ~query =
    let f uri () : (t, string) Lwt_result.t =
      let t, w = Lwt.task () in
      let socket = new%js webSocket (Js.string uri) in
      let open_handler = fun (evt : webSocket Dom.event Js.t) ->
        Js.Opt.iter evt##.target (Lwt.wakeup_later w % return_ok);
        Js._true in
      let error_handler = fun (evt : webSocket Dom.event Js.t) ->
        (* FIXME get error message from event. *)
        if Lwt.is_sleeping t
        then Lwt.wakeup_later w (Error "socket error")
        else (match on_error with Some f -> f evt | None -> ());
        Js._true in
      socket##.onopen := Dom.handler open_handler;
      socket##.onerror := Dom.handler error_handler;
      t in
    make_uri ?secure ?host ?port ~path ~query ~f

  let subscribe (f : (Body.t, string) result -> unit) (socket : t) : unit =
    let message_handler = fun (evt : webSocket messageEvent Js.t) ->
      (match Body.of_event evt with
       | Ok x -> f @@ Ok x
       | Error `Conv_error s -> f @@ Error s);
      Js._true in
    socket##.onmessage := Dom.handler message_handler

end
