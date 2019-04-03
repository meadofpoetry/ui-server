type 'a rsp = [`V of 'a | `Tm]
type 'a resolver = [`Tm | `Msgs of 'a list] -> unit
type ('a, 'b) msg = unit -> ('b rsp Lwt.t * 'a resolver)

module Pool : sig
  type ('a, 'b) t

  val create : ('a, 'b) msg list -> ('a, 'b) t

  val send : ('a, 'b) t -> ('a, 'b) t

  val apply : ('a, 'b) t -> 'a list -> ('a, 'b) t

  val is_empty : ('a, 'b) t -> bool

  val is_last : ('a, 'b) t -> bool

  val _match :
    ('a, 'b) t ->
    resolved:(('a, 'b) t -> 'b -> 'c Lwt.t) ->
    timeout:(('a, 'b) t -> 'c Lwt.t) ->
    pending:(('a, 'b) t -> 'c Lwt.t) ->
    not_sent:(('a, 'b) t -> 'c Lwt.t) ->
    'c Lwt.t
end

module Queue : sig
  type ('a, 'b) t

  val create : ('a, 'b) msg list -> ('a, 'b) t

  val send : ('a, 'b) t -> ('a, 'b) t

  val apply : ('a, 'b) t -> 'a list -> ('a, 'b) t

  val append : ('a, 'b) t -> ('a, 'b) msg list -> ('a, 'b) t

  val is_empty : ('a, 'b) t -> bool

  val _match :
    ('a, 'b) t ->
    resolved:(('a, 'b) t -> 'b -> 'c Lwt.t) ->
    timeout:(('a, 'b) t -> 'c Lwt.t) ->
    pending:(('a, 'b) t -> 'c Lwt.t) ->
    not_sent:(('a, 'b) t -> 'c Lwt.t) ->
    'c Lwt.t
end

module Await_queue : sig
  type ('a, 'b) t

  val is_empty : ('a, 'b) t -> bool

  val has_pending : ('a, 'b) t -> bool

  val create : ('a, 'b) msg list -> ('a, 'b) t

  val apply : ('a, 'b) t -> 'a list -> ('a, 'b) t

  val send : ('a, 'b) t -> ('a, 'b) t
end
