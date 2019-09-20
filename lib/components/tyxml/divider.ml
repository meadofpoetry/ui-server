module CSS = struct
  let root = "mdc-divider"

  let inset = BEM.add_modifier root "inset"
end

module Make
    (Xml : Xml_sigs.T with type ('a, 'b) W.ft = 'a -> 'b)
    (Svg : Svg_sigs.T with module Xml := Xml)
    (Html : Html_sigs.T with module Xml := Xml and module Svg := Svg) =
struct
  open Xml.W
  open Html

  let ( % ) f g x = f (g x)

  let divider
      ?(classes = return [])
      ?(a = [])
      ?(inset = false)
      ~(tag : ?a:'a attrib list -> 'b) : 'b =
    let classes = fmap (Utils.cons_if inset CSS.inset % List.cons CSS.root) classes in
    tag ~a:(a_class classes :: a)

  let divider_hr = divider ~tag:hr

  let divider_li ?classes ?a ?inset () = divider ~tag:li ?classes ?a ?inset (nil ())
end

module F = Make (Tyxml.Xml) (Tyxml.Svg) (Tyxml.Html)
