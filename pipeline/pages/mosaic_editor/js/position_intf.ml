module type S = sig
  type t

  val width : t -> float
  val height : t -> float
  val top : t -> float
  val left : t -> float

  val set_width : t -> float -> t
  val set_height : t -> float -> t
  val set_top : t -> float -> t
  val set_left : t -> float -> t

  val make : left:float -> top:float -> width:float -> height:float -> t
end

module type Position = sig

  include S

  val equal : t -> t -> bool

  val show : t -> string

  val right : t -> float

  val bottom : t -> float

  val empty : t

  val collides : t -> t -> bool

  val validate_left_top :
    ?min_left:float
    -> ?min_top:float
    -> ?max_left:float
    -> ?max_top:float
    -> ?parent_width:float
    -> ?parent_height:float
    -> t
    -> t

  val validate :
    ?min_left:float
    -> ?min_top:float
    -> ?max_left:float
    -> ?max_top:float
    -> ?max_width:float
    -> ?min_width:float
    -> ?max_height:float
    -> ?min_height:float
    -> ?parent_width:float
    -> ?parent_height:float
    -> t
    -> t

  val bounding_rect : t list -> t
end
