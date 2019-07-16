open Js_of_ocaml

include module type of Page_mosaic_editor_tyxml.Position

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
