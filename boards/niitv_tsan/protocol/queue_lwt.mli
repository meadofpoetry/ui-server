exception Full

exception Empty

type 'a t

val create : int -> 'a t * ('a -> unit Lwt.t)

val clear : 'a t -> unit Lwt.t

val length : 'a t -> int

val next : 'a t -> 'a Lwt.t

val get_while : ('a -> bool) -> 'a t -> 'a list Lwt.t
