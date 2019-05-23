exception Full

exception Empty

type 'a t

val create : int -> ('a -> bool) -> 'a t * ('a -> unit Lwt.t)

val clear : 'a t -> unit Lwt.t

val length : 'a t -> int

val check_condition : 'a t -> unit Lwt.t

val next : 'a t -> 'a Lwt.t
