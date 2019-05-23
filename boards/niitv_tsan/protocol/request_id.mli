type t = Uint16.t

include module type of Uint16 with type t := t

val next : unit -> t
