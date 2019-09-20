module CSS = struct
  (** Mandatory. *)
  let root = "mdc-line-ripple"

  (** Styles the line ripple as an active line ripple. *)
  let active = BEM.add_modifier root "active"

  (** Styles the line ripple as a deactivating line ripple. *)
  let deactivating = BEM.add_modifier root "deactivating"
end

module Make
    (Xml : Xml_sigs.T with type ('a, 'b) W.ft = 'a -> 'b)
    (Svg : Svg_sigs.T with module Xml := Xml)
    (Html : Html_sigs.T with module Xml := Xml and module Svg := Svg) =
struct
  open Xml.W
  open Html

  let line_ripple ?(classes = return []) ?(a = []) ?(children = nil ()) () =
    let classes = fmap (List.cons CSS.root) classes in
    div ~a:(a_class classes :: a) children
end

module F = Make (Tyxml.Xml) (Tyxml.Svg) (Tyxml.Html)
