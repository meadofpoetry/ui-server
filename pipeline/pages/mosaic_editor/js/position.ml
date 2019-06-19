open Js_of_ocaml
open Components

module Attr = struct
  let keep_aspect_ratio = "data-keep-aspect-ratio"
  let aspect_ratio = "data-aspect-ratio"
  let width = "data-width"
  let height = "data-height"
  let left = "data-left"
  let top = "data-top"
end

type t =
  { x : int
  ; y : int
  ; w : int
  ; h : int
  }

let empty =
  { x = 0
  ; y = 0
  ; w = 0
  ; h = 0
  }

let apply_to_element (pos : t) (elt : #Dom_html.element Js.t) =
  let min_size = 20 in (* FIXME *)
  if pos.w > min_size
  then (
    elt##.style##.width := Utils.px_js pos.w;
    elt##.style##.left := Utils.px_js pos.x);
  if pos.h > min_size
  then (
    elt##.style##.height := Utils.px_js pos.h;
    elt##.style##.top := Utils.px_js pos.y)

let of_element (elt : #Dom_html.element Js.t) =
  { x = elt##.offsetLeft
  ; y = elt##.offsetTop
  ; w = elt##.offsetWidth
  ; h = elt##.offsetHeight
  }

let to_client_rect (t : t) : Dom_html.clientRect Js.t =
  object%js
    val top = float_of_int t.y
    val left = float_of_int t.x
    val right = float_of_int @@ t.x + t.w
    val bottom = float_of_int @@ t.y + t.h
    val width = Js.def @@ float_of_int t.w
    val height = Js.def @@ float_of_int t.h
  end

let of_client_rect (rect : Dom_html.clientRect Js.t) : t =
  { x = int_of_float rect##.left
  ; y = int_of_float rect##.top
  ; w = Js.Optdef.case rect##.width (fun () -> 0) int_of_float
  ; h = Js.Optdef.case rect##.height (fun () -> 0) int_of_float
  }

let default_aspect_ratio = 1.

let string_of_float = Printf.sprintf "%g"

let get_int_attribute (elt : #Dom_html.element Js.t) attr : int =
  match Element.get_attribute elt attr with
  | None -> 0
  | Some x ->
    match int_of_string_opt x with
    | None -> 0
    | Some x -> x

let get_original_width (elt : #Dom_html.element Js.t) : int =
  get_int_attribute elt Attr.width

let get_original_height (elt : #Dom_html.element Js.t) : int =
  get_int_attribute elt Attr.height

let get_original_left (elt : #Dom_html.element Js.t) : int =
  get_int_attribute elt Attr.left

let get_original_top (elt : #Dom_html.element Js.t) : int =
  get_int_attribute elt Attr.top

let get_original_aspect_ratio (elt : #Dom_html.element Js.t) : float option =
  match Element.get_attribute elt Attr.aspect_ratio with
  | Some x -> Some (float_of_string x)
  | None ->
    let w, h = get_original_width elt, get_original_height elt in
    if w = 0 || h = 0
    then None
    else
      let ar = (float_of_int w) /. (float_of_int h) in
      Element.set_attribute elt Attr.aspect_ratio (string_of_float ar);
      Some ar
