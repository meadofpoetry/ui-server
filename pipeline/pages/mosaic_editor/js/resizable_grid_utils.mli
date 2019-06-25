open Js_of_ocaml

type event = Touch of Dom_html.touchEvent Js.t
           | Mouse of Dom_html.mouseEvent Js.t

type value =
  [ `Auto
  | `Px of float
  | `Fr of float
  | `Pc of float
  ]

class type stylish =
  object
    method style : Dom_html.cssStyleDeclaration Js.t Js.prop
  end

val value_of_string : string -> value

val value_of_string_opt : string -> value option

val value_to_string : value -> string

val pp_value : Format.formatter -> value -> unit

val coerce_event : event -> Dom_html.event Js.t

val cell_of_event : Dom_html.element Dom.nodeList Js.t
  -> Dom_html.event Js.t
  -> Dom_html.element Js.t option

val get_cursor_position : ?touch_id:int -> event -> int * int

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
