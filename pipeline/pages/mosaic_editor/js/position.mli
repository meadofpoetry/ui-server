open Js_of_ocaml

include module type of Page_mosaic_editor_tyxml.Position

type line =
  { is_vertical : bool (* Is line vertical *)
  ; is_multiple : bool (* Multiple intersection detected *)
  ; is_center : bool
  ; origin : float
  }

type aspect = int * int

val fix_aspect : t -> int * int -> t

val apply_to_element : unit:[`Px | `Pct | `Norm] -> t -> #Dom_html.element Js.t -> unit

val of_element : #Dom_html.element Js.t -> t

val of_client_rect : Dom_html.clientRect Js.t -> t

val bounding_rect : t list -> t

val adjust :
  ?aspect_ratio:aspect (* Aspect ratio of active item, if any *)
  -> ?snap_lines:bool
  -> ?collisions:bool
  -> ?min_width:float
  -> ?min_height:float
  -> ?min_distance:float
  -> ?grid_step:float
  -> ?max_width:float
  -> ?max_height:float
  -> action:[`Resize of direction | `Move]
  -> siblings:t list (* Active item neighbours (with active item) *)
  -> parent_size:float * float (* Parent width & height *)
  -> frame_position:t
  -> t list
  -> t * t list * line list (* Adjusted position & lines properties *)
