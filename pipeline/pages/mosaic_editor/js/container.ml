open Js_of_ocaml
open Pipeline_types

type halign = Left | HCenter | Right
type valign = Top | VCenter | Bottom
type align = halign * valign

let container_of_cell (elt : Dom_html.element Js.t) : Wm.container =
  (* TODO implement. Seems that it is better to use relative
     dimensions like 'fr' rather than absolute pixel values. *)
  let width = elt##.offsetWidth in
  let height = elt##.offsetHeight in
  let position =
    { Wm. left = 0
    ; right = width
    ; top = 0
    ; bottom = height
    } in
  { position
  ; widgets = []
  }

let min_size (container : Wm.container) =
  failwith "TODO"

let scale (container : Wm.container)
    (width, height)
    (align : align) =
  failwith "TODO"
