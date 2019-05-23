type t

val max_int : t

val zero : t

val equal : t -> t -> bool

val compare : t -> t -> int

val add : t -> t -> t

val sub : t -> t -> t

val succ : t -> t

val pred : t -> t

val to_int : t -> int

val of_int : int -> t

val pp : Format.formatter -> t -> unit
