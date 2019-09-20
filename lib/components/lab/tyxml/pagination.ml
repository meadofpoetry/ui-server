open Components_tyxml

module CSS = struct
  let root = "mdc-pagination"

  let content = BEM.add_element root "content"

  let button = BEM.add_element root "button"

  let button_active = BEM.add_modifier button "active"
end

module Make
    (Xml : Xml_sigs.T with type ('a, 'b) W.ft = 'a -> 'b)
    (Svg : Svg_sigs.T with module Xml := Xml)
    (Html : Html_sigs.T with module Xml := Xml and module Svg := Svg) =
struct
  open Xml.W
  open Html

  let pagination_content ?(classes = return []) ?(a = []) content =
    let classes = fmap (fun x -> CSS.content :: x) classes in
    div ~a:(a_class classes :: a) content

  let pagination ?(classes = return []) ?(a = []) content =
    let classes = fmap (fun x -> CSS.root :: x) classes in
    div ~a:(a_class classes :: a) content
end

module F = Make (Tyxml.Xml) (Tyxml.Svg) (Tyxml.Html)
