include module type of Ptime with type t = Ptime.t and type span = Ptime.span

val to_human_string : ?tz_offset_s:tz_offset_s -> t -> string

val of_human_string_exn : ?tz_offset_s:tz_offset_s -> string -> t

val to_yojson : t -> Yojson.Safe.t

val of_yojson : Yojson.Safe.t -> (t, string) result

val split : from:t -> till:t -> (t * t) list

module Interval : sig

  module type Time_now = sig
    val now : unit -> Ptime.t
  end

  module Make : functor (M : Time_now) ->
  sig
    val make :
      ?from:Ptime.t ->
      ?till:Ptime.t ->
      ?duration:Ptime.span ->
      unit -> ([> `Range of Ptime.t * Ptime.t ], string) result
  end

end

module Hours : sig
  type nonrec t = t
  val of_int : int -> t
  val to_int : t -> int
  val of_string : string -> t
  val to_string : t -> string
  val of_yojson : Yojson.Safe.t -> (t, string) result
  val to_yojson : t -> Yojson.Safe.t
end

module Seconds : sig
  type nonrec t = t
  val of_int : int -> t
  val to_int : t -> int
  val of_string : string -> t
  val to_string : t -> string
  val of_yojson : Yojson.Safe.t -> (t, string) result
  val to_yojson : t -> Yojson.Safe.t
end

module Seconds64 : sig
  type nonrec t = t
  val of_int64 : int64 -> t
  val to_int64 : t -> int64
  val of_string : string -> t
  val to_string : t -> string
  val of_yojson : Yojson.Safe.t -> (t, string) result
  val to_yojson : t -> Yojson.Safe.t
end

module Useconds : sig
  type nonrec t = t
  val of_int64 : int64 -> t
  val to_int64 : t -> int64
  val of_string : string -> t
  val to_string : t -> string
  val of_yojson : Yojson.Safe.t -> (t, string) result
  val to_yojson : t -> Yojson.Safe.t
end

module Period : sig
  include module type of Ptime.Span

  val to_yojson : t -> Yojson.Safe.t

  val of_yojson : Yojson.Safe.t -> (t, string) result
    
  module Hours : sig
    type nonrec t = t
    val of_int : int -> t
    val to_int : t -> int
    val of_string : string -> t
    val to_string : t -> string
    val of_yojson : Yojson.Safe.t -> (t, string) result
    val to_yojson : t -> Yojson.Safe.t
  end
       
  module Seconds : sig
    type nonrec t = t
    val of_int : int -> t
    val to_int : t -> int
    val of_string : string -> t
    val to_string : t -> string
    val of_yojson : Yojson.Safe.t -> (t, string) result
    val to_yojson : t -> Yojson.Safe.t
  end
       
  module Seconds64 : sig
    type nonrec t = t
    val of_int64 : int64 -> t
    val to_int64 : t -> int64
    val of_string : string -> t
    val to_string : t -> string
    val of_yojson : Yojson.Safe.t -> (t, string) result
    val to_yojson : t -> Yojson.Safe.t
  end

  module Useconds : sig
    type nonrec t = t
    val of_int64 : int64 -> t
    val to_int64 : t -> int64
    val of_string : string -> t
    val to_string : t -> string
    val of_yojson : Yojson.Safe.t -> (t, string) result
    val to_yojson : t -> Yojson.Safe.t
  end

end

module Range : sig

  type nonrec t = t * Period.t

  val after : Ptime.t -> Period.t -> t
  val equal : t -> t -> bool
  val of_yojson : Yojson.Safe.t -> (t, string) result
  val to_yojson : t -> Yojson.Safe.t

end

module Relative : sig
  include module type of Ptime.Span

  val to_seconds : t -> int

  val of_seconds : int -> t

end
