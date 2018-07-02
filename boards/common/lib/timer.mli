type t
exception Timeout of t
val create : step:float -> int -> t
val reset  : t -> t
val period : t -> int
val step   : t -> t
