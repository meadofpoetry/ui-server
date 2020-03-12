open Application_types
open Board_niitv_tsan_types

type t

val create : ?period:Ptime.span -> unit -> t

val clear_stream : t -> Stream.ID.t -> unit

val clear : t -> unit

val set_period : t -> Ptime.span -> unit

val map :
  t -> (Stream.ID.t * Bitrate.cur) list -> (Stream.ID.t * Bitrate.ext) list
