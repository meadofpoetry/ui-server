include module type of Position_intf

module Make : functor(Pos : S) -> Position with type t = Pos.t

type absolute =
  { x : float
  ; y : float
  ; w : float
  ; h : float
  }

module Normalized : sig
  include Position with type t = Pipeline_types.Wm.position

  val validate : t -> t

  val compare : t -> t -> int

  val apply_to_element : t -> Js_of_ocaml.Dom_html.element Js_of_ocaml.Js.t -> unit

  val of_element : #Js_of_ocaml.Dom_html.element Js_of_ocaml.Js.t -> t

end

module Absolute : sig
  include Position with type t = absolute

  val apply_to_element : t -> Js_of_ocaml.Dom_html.element Js_of_ocaml.Js.t -> unit

  val of_element : #Js_of_ocaml.Dom_html.element Js_of_ocaml.Js.t -> t

  val of_client_rect : Js_of_ocaml.Dom_html.clientRect Js_of_ocaml.Js.t -> t

  val adjust :
    ?aspect_ratio:int * int
    -> ?snap_lines:bool
    -> ?min_width:float
    -> ?min_height:float
    -> ?min_distance:float
    -> ?grid_step:float
    -> ?max_width:float
    -> ?max_height:float
    -> action:[`Resize of Page_mosaic_editor_tyxml.Direction.t | `Move]
    -> siblings:t list
    -> parent_size:float * float
    -> frame_position:t
    -> t list
    -> t * t list * Snap_line.t list
end

val absolute_to_normalized : parent_size:float * float -> Absolute.t -> Normalized.t
