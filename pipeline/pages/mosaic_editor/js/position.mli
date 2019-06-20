open Js_of_ocaml

module Attr : sig
  val keep_aspect_ratio : string
  val aspect_ratio : string
  val width : string
  val height : string
  val left : string
  val top : string
end

type line =
  { is_vertical : bool (* Is line vertical *)
  ; is_multiple : bool (* Multiple intersection detected *)
  ; is_center : bool
  ; x : int
  ; y : int
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

val apply_to_element : ?min_size:int -> t -> #Dom_html.element Js.t -> unit

val of_element : #Dom_html.element Js.t -> t

val to_client_rect : t -> Dom_html.clientRect Js.t

val of_client_rect : Dom_html.clientRect Js.t -> t

val get_original_width : #Dom_html.element Js.t -> int

val get_original_height : #Dom_html.element Js.t -> int

val get_original_left : #Dom_html.element Js.t -> int

val get_original_top : #Dom_html.element Js.t -> int

val get_original_aspect_ratio : #Dom_html.element Js.t -> float option

val adjust :
  ?aspect_ratio:float (* Aspect ratio of active item, if any *)
  -> action:[`Resize of resize_direction | `Move]
  -> original_position:t
  -> position:t (* Active item position *)
  -> siblings:Dom_html.element Js.t list (* Active item neighbours (with active item) *)
  -> parent_size:int * int (* Parent width & height *)
  -> Dom_html.element Js.t (* Active item *)
  -> t * (line list) (* Adjusted position & lines properties *)

val scale :
  original_parent_size:int * int
  -> parent_size:int * int
  -> t
  -> t

val find_spare :
  ?compare:(t -> t -> int)
  -> ?aspect:int * int
  -> ?min_w:int
  -> ?min_h:int
  -> ?max_w:int
  -> ?max_h:int
  -> siblings:t list
  -> parent_size:int * int
  -> int * int (* Cursor position relative to parent rect *)
  -> t option
