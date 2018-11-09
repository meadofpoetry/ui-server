module Point : sig
  type t
  val radius : t -> int
  val set_radius : t -> int -> unit [@@js.set "radius"]
  val point_style : t -> string [@@js.get "pointStyle"]

end
