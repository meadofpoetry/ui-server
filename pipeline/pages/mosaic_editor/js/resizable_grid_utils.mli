open Js_of_ocaml
open Js_of_ocaml_tyxml
open Page_mosaic_editor_tyxml.Resizable_grid

type direction = Col | Row

type cell_position =
  { col : int
  ; row : int
  ; col_span : int
  ; row_span : int
  }

val make_cell_position :
  ?col_span:int
  -> ?row_span:int
  -> col:int
  -> row:int
  -> unit
  -> cell_position

type event = Touch of Dom_html.touchEvent Js.t
           | Mouse of Dom_html.mouseEvent Js.t

class type stylish =
  object
    method style : Dom_html.cssStyleDeclaration Js.t Js.prop
  end

val coerce_event : event -> Dom_html.event Js.t

val cell_of_event : Dom_html.element Js.t list
  -> #Dom_html.event Js.t
  -> Dom_html.element Js.t option

val get_cursor_position : ?touch_id:int -> event -> int * int

val insert_at_idx : int -> 'a -> 'a list -> 'a list

val remove_at_idx : int -> 'a list -> 'a list

(** Returns the number of pixels in one frame *)
val fr_to_pixels :
  value array
  -> float array
  -> float

(** Returns the number of pixels in one percent *)
val percentage_to_pixels :
  value array
  -> float array
  -> float

(** Returns track size in pixels *)
val get_size_at_track : ?gap:float -> float array -> float

val get_styles : string -> Dom_html.element Js.t -> string list

val get_cell_position : Dom_html.element Js.t -> cell_position

val set_cell_col : ?span:int -> int -> Dom_html.element Js.t -> unit

val set_cell_row : ?span:int -> int -> Dom_html.element Js.t -> unit

val set_cell_position : cell_position -> Dom_html.element Js.t -> unit

val find_first_cell : direction
  -> int
  -> Dom_html.element Js.t list
  -> Dom_html.element Js.t

val get_parent_grid : Dom_html.element Js.t -> Dom_html.element Js.t

val gen_cells :
  f:(col:int -> row:int -> unit -> ([> Html_types.div] Tyxml_js.Html.elt as 'a))
  -> rows:int
  -> cols:int
  -> 'a list

(* Checks if a group of cells can be merged into one cell *)
val is_merge_possible : Dom_html.element Js.t list -> bool

val get_topmost_cell : Dom_html.element Js.t list -> Dom_html.element Js.t

val get_bottommost_cell : Dom_html.element Js.t list -> Dom_html.element Js.t

val get_leftmost_cell : Dom_html.element Js.t list -> Dom_html.element Js.t

val get_rightmost_cell : Dom_html.element Js.t list -> Dom_html.element Js.t
