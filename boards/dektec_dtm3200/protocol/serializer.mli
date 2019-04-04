val calc_crc : Cstruct.t -> int

val make_get : 'a Request.t -> Cstruct.t

module Set : sig
  val bool : 'a Request.t -> bool -> Cstruct.t

  val int8 : 'a Request.t -> int -> Cstruct.t

  val int16 : 'a Request.t -> int -> Cstruct.t

  val int32 : 'a Request.t -> int32 -> Cstruct.t

  val ipaddr : 'a Request.t -> Ipaddr.V4.t -> Cstruct.t
end
