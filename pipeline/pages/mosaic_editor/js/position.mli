open Js_of_ocaml

type line =
  { is_vertical : bool (* Is line vertical *)
  ; is_multiple : bool (* Multiple intersection detected *)
  ; is_center : bool
  ; origin : int
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
  { x : int
  ; y : int
  ; w : int
  ; h : int
  }

val empty : t

val show : t -> string

val equal : t -> t -> bool

val compare : t -> t -> int

val apply_to_element : t -> #Dom_html.element Js.t -> unit

val of_element : #Dom_html.element Js.t -> t

val to_client_rect : t -> Dom_html.clientRect Js.t

val of_client_rect : Dom_html.clientRect Js.t -> t

val adjust :
  ?aspect_ratio:(int * int) (* Aspect ratio of active item, if any *)
  -> ?snap_lines:bool
  -> ?collisions:bool
  -> ?min_width:int
  -> ?min_height:int
  -> ?max_width:int
  -> ?max_height:int
  -> action:[`Resize of resize_direction | `Move]
  -> original_position:t
  -> position:t (* Active item position *)
  -> siblings:Dom_html.element Js.t list (* Active item neighbours (with active item) *)
  -> parent_size:int * int (* Parent width & height *)
  -> Dom_html.element Js.t (* Active item *)
  -> t * (line list) (* Adjusted position & lines properties *)

val of_wm_position :
  ?parent_aspect:int * int
  -> parent_position:Pipeline_types.Wm.position
  -> Pipeline_types.Wm.position
  -> t

val to_wm_position :
  ?parent_aspect:int * int
  -> parent_position:Pipeline_types.Wm.position
  -> t
  -> Pipeline_types.Wm.position
