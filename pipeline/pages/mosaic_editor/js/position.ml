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

let default_aspect_ratio = 1.

let string_of_float = Printf.sprintf "%g"

let get_int_attribute (elt : #Dom_html.element Js.t) attr : int =
  match Element.get_attribute elt attr with
  | None -> 0
  | Some x ->
    match int_of_string_opt x with
    | None -> 0
    | Some x -> x

let get_width (elt : #Dom_html.element Js.t) : int =
  get_int_attribute elt Attr.width

let get_height (elt : #Dom_html.element Js.t) : int =
  get_int_attribute elt Attr.height

let get_left (elt : #Dom_html.element Js.t) : int =
  get_int_attribute elt Attr.left

let get_top (elt : #Dom_html.element Js.t) : int =
  get_int_attribute elt Attr.top

let get_aspect_ratio (elt : #Dom_html.element Js.t) : float =
  match Element.get_attribute elt Attr.aspect_ratio with
  | Some x -> float_of_string x
  | None ->
    let w, h = get_width elt, get_height elt in
    if w = 0 || h = 0
    then default_aspect_ratio
    else
      let ar = (float_of_int w) /. (float_of_int h) in
      Element.set_attribute elt Attr.aspect_ratio (string_of_float ar);
      ar

let get_width_for_height (elt : #Dom_html.element Js.t) (h : float) : int =
  let ar = get_aspect_ratio elt in
  (* HACK to eliminate the floating point error. Rewrite later *)
  let width = float_of_string @@ Printf.sprintf "%g" @@ h *. ar in
  (* XXX prove using round is ok *)
  Float.to_int @@ Js.math##floor width

let get_height_for_width (elt : #Dom_html.element Js.t) (w : float) : int =
  let ar = get_aspect_ratio elt in
  (* HACK to eliminate the floating point error. Rewrite later *)
  let height = float_of_string @@ Printf.sprintf "%g" @@ w /. ar in
  (* XXX prove using round is ok *)
  Float.to_int @@ Js.math##floor height
