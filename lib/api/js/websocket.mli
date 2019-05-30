open Js_of_ocaml
open Js_of_ocaml.WebSockets

type 'a t

val close_socket : 'a t -> unit

module type BODY = sig
  include Api.BODY
  val of_event : webSocket messageEvent Js.t -> (t, [ `Conv_error of string]) result
end

module type CONTROL_MSG = sig
  type t
  val parse : t -> (int * t) option
  val compose : int -> string -> t
end

module type WS = sig
  open Netlib

  type body

  type nonrec t = body t

  val close_socket : t -> unit

  val subscribe : path:('b, 'c) Uri.Path.Format.t
    -> query:('c, (body -> ('a, string) result) -> t -> 'a React.event) Uri.Query.format
    -> 'b

  val open_socket : ?secure:bool
    -> ?host:string
    -> ?port:int
    -> path:('a, unit -> ((t, string) Lwt_result.t)) Uri.Path.Format.t
    -> 'a
end

module Make (Body : BODY) (Msg : CONTROL_MSG with type t = Body.t)
  : WS with type body := Body.t

module Json_body : BODY with type t = Yojson.Safe.json

module Json_msg : CONTROL_MSG with type t = Yojson.Safe.json

module JSON : WS with type body := Yojson.Safe.json
