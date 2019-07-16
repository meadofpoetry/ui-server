open Js_of_ocaml

type line =
  { is_vertical : bool (* Is line vertical *)
  ; is_multiple : bool (* Multiple intersection detected *)
  ; is_center : bool
  ; origin : float
  }

type resize_direction =
  | Top_left
  | Top_right
  | Bottom_left
  | Bottom_right
  | Top
  | Bottom
  | Left
  | Right

(* XXX Should use float type *)
type t =
  { x : float
  ; y : float
  ; w : float
  ; h : float
  }

val empty : t

val show : t -> string

val equal : t -> t -> bool

val compare : t -> t -> int

val fix_aspect : t -> int * int -> t

val apply_to_element : ?unit:[`Px | `Pc] -> t -> #Dom_html.element Js.t -> unit

val of_element : #Dom_html.element Js.t -> t

val to_client_rect : t -> Dom_html.clientRect Js.t

val of_client_rect : Dom_html.clientRect Js.t -> t

val adjust :
  ?aspect_ratio:(int * int) (* Aspect ratio of active item, if any *)
  -> ?snap_lines:bool
  -> ?collisions:bool
  -> ?min_width:float
  -> ?min_height:float
  -> ?min_distance:float
  -> ?grid_step:float
  -> ?max_width:float
  -> ?max_height:float
  -> action:[`Resize of resize_direction | `Move]
  -> original_position:t
  -> position:t (* Active item position *)
  -> siblings:Dom_html.element Js.t list (* Active item neighbours (with active item) *)
  -> parent_size:float * float (* Parent width & height *)
  -> Dom_html.element Js.t (* Active item *)
  -> t * (line list) (* Adjusted position & lines properties *)

val to_wm_position : t -> Pipeline_types.Wm.position

val of_wm_position : Pipeline_types.Wm.position -> t

val to_relative : parent_size:(float * float) -> t -> t

val of_relative : parent_size:(float * float) -> t -> t
