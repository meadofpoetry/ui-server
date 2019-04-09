type ('a, 'b) msg

val make_msg :
  send:(unit -> unit Lwt.t) ->
  timeout:(unit -> unit Lwt.t) ->
  resolve:('a -> 'b option) ->
  unit -> ('a, 'b) msg

module Pool : sig
  type e = [ `Timeout ]

  type ('a, 'b) t

  val create : ('a, 'b) msg list -> ('a, 'b) t

  val apply : ('a, 'b) t -> 'a list -> unit

  val append : ('a, 'b) t -> ('a, 'b) msg list -> ('a, 'b) t

  val is_empty : ('a, 'b) t -> bool

  val is_last : ('a, 'b) t -> bool

  val send : ('a, 'b) t -> ('a, 'b) t Lwt.t

  val _match :
    ('a, 'b) t ->
    resolved:(('a, 'b) t -> 'b -> 'c Lwt.t) ->
    error:(('a, 'b) t -> e -> 'c Lwt.t) ->
    pending:(('a, 'b) t -> 'c Lwt.t) ->
    not_sent:(('a, 'b) t -> 'c Lwt.t) -> 'c Lwt.t
end

module Queue : sig
  type e = [`Timeout | `Interrupted]

  type ('a, 'b) t

  val create : ('a, 'b) msg list -> ('a, 'b) t

  val send : ('a, 'b) t -> ('a, 'b) t Lwt.t

  val cons : ('a, 'b) t -> ('a, 'b) msg -> ('b, e) result Lwt.t * ('a, 'b) t

  val snoc : ('a, 'b) t -> ('a, 'b) msg -> ('b, e) result Lwt.t * ('a, 'b) t

  val append : ('a, 'b) t -> ('a, 'b) msg list -> ('a, 'b) t

  val apply : ('a, 'b) t -> 'a list -> unit

  val is_empty : ('a, 'b) t -> bool

  val invalidate : ('a, 'b) t -> ('a, 'b) t

  val _match :
    ('a, 'b) t ->
    resolved:(('a, 'b) t -> 'b -> 'c Lwt.t) ->
    error:(('a, 'b) t -> e -> 'c Lwt.t) ->
    pending:(('a, 'b) t -> 'c Lwt.t) ->
    not_sent:(('a, 'b) t -> 'c Lwt.t) ->
    'c Lwt.t
end
