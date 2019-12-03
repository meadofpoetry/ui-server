(** @see <https://github.com/davidtheclark/tabbable>.
    [tabbable] JavaScript library rewritten in OCaml. *)

open Js_of_ocaml

val is_tabbable : #Dom_html.element Js.t -> bool
(** Returns a boolean indicating whether the provided node
    is considered tabbable. *)

val is_focusable : #Dom_html.element Js.t -> bool
(** Returns a boolean indicating whether the provided node
    is considered focusable. *)

val get_tabbable :
  ?include_container:bool -> #Dom_html.element Js.t -> Dom_html.element Js.t list
(** Returns an array of ordered tabbable node within the root node.
    Summary of ordering principles:
    - First include any nodes with positive [tabindex] attributes (1 or higher),
      ordered by ascending [tabindex] and source order.
    - Then include any nodes with a zero [tabindex] and any element that by
      default receives focus (listed above) and does not have a positive [tabindex]
      set, in source order.

    @param include_container if set to [true], the root node will be
    included in the returned tabbable node array, if root node is
    tabbable. *)
