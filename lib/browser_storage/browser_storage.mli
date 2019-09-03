open Js_of_ocaml

type key = string

val put : Dom_html.storage Js.t -> key -> Yojson.Safe.t -> unit

val get : Dom_html.storage Js.t -> key -> Yojson.Safe.t option

val remove : Dom_html.storage Js.t -> key -> unit

val clear : Dom_html.storage Js.t -> unit

module type STORAGE = sig
  val is_available : bool

  val put : key -> Yojson.Safe.t -> unit

  val get : key -> Yojson.Safe.t option

  val remove : key -> unit

  val clear : unit -> unit
end

module Local : STORAGE

module Session : STORAGE

module type USER = sig
  type t

  val to_string : t -> string
end

module type USER_STORAGE = sig
  type user

  val is_available : bool

  val put : user -> key -> Yojson.Safe.t -> unit

  val get : user -> key -> Yojson.Safe.t option

  val remove : user -> key -> unit

  val clear : user -> unit
end

module Make (S : STORAGE) (U : USER) : USER_STORAGE with type user := U.t

module Make_session (U : USER) : USER_STORAGE with type user := U.t

module Make_local (U : USER) : USER_STORAGE with type user := U.t
