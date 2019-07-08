open Js_of_ocaml

(* XXX Can be rewritten as a flat variant if needed,
   like Top_left | Top | Top_right ... *)
type halign = Left | HCenter | Right
type valign = Top | VCenter | Bottom
type align = halign * valign

val of_element : Dom_html.element Js.t -> Pipeline_types.Wm.container

(** Returns the minimum avaiable size for the container *)
val min_size :
  Pipeline_types.Wm.container
  -> int * int

(** Updates positions of inner widgets of the container
    according to the provided alignment and container size *)
val scale :
  Pipeline_types.Wm.container (* Original container properties *)
  -> int * int (* New width & height of the container *)
  -> align (* Content align *)
  -> Pipeline_types.Wm.container (* Updated container properties *)
