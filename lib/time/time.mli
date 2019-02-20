include module type of Ptime

val to_human_string : ?tz_offset_s:tz_offset_s -> t -> string

val of_human_string_exn : ?tz_offset_s:tz_offset_s -> string -> t

val to_yojson : t -> Yojson.Safe.json

val of_yojson : Yojson.Safe.json -> (t, string) result

val make_interval : ?from:t
                    -> ?till:t
                    -> ?duration:span
                    -> unit
                    -> ([> `Range of t * t ], string) result

val split : from:t -> till:t -> (t * t) list

module Period : sig
  include module type of Ptime.Span

  val to_yojson : t -> Yojson.Safe.json

  val of_yojson : Yojson.Safe.json -> (t, string) result
    
  module Hours : sig
    type nonrec t = t
    val of_int : int -> t
    val to_int : t -> int
    val of_string : string -> t
    val to_string : t -> string
    val of_yojson : Yojson.Safe.json -> (t, string) result
    val to_yojson : t -> Yojson.Safe.json
  end
       
  module Seconds : sig
    type nonrec t = t
    val of_int : int -> t
    val to_int : t -> int
    val of_string : string -> t
    val to_string : t -> string
    val of_yojson : Yojson.Safe.json -> (t, string) result
    val to_yojson : t -> Yojson.Safe.json
  end
       
  module Seconds64 : sig
    type nonrec t = t
    val of_int64 : int64 -> t
    val to_int64 : t -> int64
    val of_string : string -> t
    val to_string : t -> string
    val of_yojson : Yojson.Safe.json -> (t, string) result
    val to_yojson : t -> Yojson.Safe.json
  end

  module Useconds : sig
    type nonrec t = t
    val of_int64 : int64 -> t
    val to_int64 : t -> int64
    val of_string : string -> t
    val to_string : t -> string
    val of_yojson : Yojson.Safe.json -> (t, string) result
    val to_yojson : t -> Yojson.Safe.json
  end

end

module Range : sig

  type nonrec t = t * Period.t

  val after : Ptime.t -> Period.t -> t

end

module Relative : sig
  include module type of Ptime.Span

  val to_seconds : t -> int

  val of_seconds : int -> t

end
