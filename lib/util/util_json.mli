include module type of Yojson.Safe

val equal : t -> t -> bool

val compare : t -> t -> int

val pp : Format.formatter -> t -> unit

val to_yojson : t -> t

val of_yojson : t -> (t, string) result

module Bool : sig
  type t = bool

  val to_yojson : t -> Yojson.Safe.t

  val of_yojson : Yojson.Safe.t -> (t, string) result
end

module Float : sig
  type t = float

  val to_yojson : t -> Yojson.Safe.t

  val of_yojson : Yojson.Safe.t -> (t, string) result
end

module Int : sig
  type t = int

  val to_yojson : t -> Yojson.Safe.t

  val of_yojson : Yojson.Safe.t -> (t, string) result
end

module Int32 : sig
  type t = int32

  val to_yojson : t -> Yojson.Safe.t

  val of_yojson : Yojson.Safe.t -> (t, string) result
end

module Int64 : sig
  type t = int64

  val to_yojson : t -> Yojson.Safe.t

  val of_yojson : Yojson.Safe.t -> (t, string) result
end

module String : sig
  type t = string

  val to_yojson : t -> Yojson.Safe.t

  val of_yojson : Yojson.Safe.t -> (t, string) result
end

module List : sig
  type 'a t = 'a list

  val to_yojson : ('a -> Yojson.Safe.t) -> 'a t -> Yojson.Safe.t

  val of_yojson :
    (Yojson.Safe.t -> ('a, string) result) ->
    Yojson.Safe.t ->
    ('a t, string) result
end

module Option : sig
  type 'a t = 'a option

  val to_yojson : ('a -> Yojson.Safe.t) -> 'a t -> Yojson.Safe.t

  val of_yojson :
    (Yojson.Safe.t -> ('a, string) result) ->
    Yojson.Safe.t ->
    ('a t, string) result
end

module Result : sig
  type ('a, 'b) t = ('a, 'b) result

  val to_yojson :
    ('a -> Yojson.Safe.t) ->
    ('b -> Yojson.Safe.t) ->
    ('a, 'b) t ->
    Yojson.Safe.t

  val of_yojson :
    (Yojson.Safe.t -> ('a, string) result) ->
    (Yojson.Safe.t -> ('b, string) result) ->
    Yojson.Safe.t ->
    (('a, 'b) t, string) result
end

module Pair : sig
  type ('a, 'b) t = 'a * 'b

  val to_yojson :
    ('a -> Yojson.Safe.t) ->
    ('b -> Yojson.Safe.t) ->
    ('a, 'b) t ->
    Yojson.Safe.t

  val of_yojson :
    (Yojson.Safe.t -> ('a, string) result) ->
    (Yojson.Safe.t -> ('b, string) result) ->
    Yojson.Safe.t ->
    (('a, 'b) t, string) result
end
