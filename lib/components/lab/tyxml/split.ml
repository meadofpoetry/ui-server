module CSS = struct
  open Components_tyxml

  let root = "mdc-split"

  let horizontal = BEM.add_modifier root "horizontal"

  let vertical = BEM.add_modifier root "vertical"

  let panel = BEM.add_element root "panel"

  let splitter = BEM.add_element root "splitter"

  let splitter_icon = BEM.add_element root "splitter-icon"
end

module Make
    (Xml : Xml_sigs.T with type ('a, 'b) W.ft = 'a -> 'b)
    (Svg : Svg_sigs.T with module Xml := Xml)
    (Html : Html_sigs.T with module Xml := Xml and module Svg := Svg) =
struct
  open Xml.W
  open Html

  let ( @:: ) = cons

  let split_panel ?(classes = return []) ?(a = []) ?(children = nil ()) () =
    let classes = fmap (fun x -> CSS.panel :: x) classes in
    div ~a:(a_class classes :: a) children

  let split_splitter ?(classes = return []) ?(a = []) () =
    let classes = fmap (fun x -> CSS.splitter :: x) classes in
    let icon =
      div
        ~a:[a_class (return [CSS.splitter_icon])]
        (return (div (nil ())) @:: return (div (nil ())) @:: nil ())
    in
    div ~a:(a_class classes :: a) (singleton (return icon))

  let split
      ?(classes = return [])
      ?(a = [])
      ?(vertical = false)
      ?(splitter = split_splitter ())
      ~panel_a
      ~panel_b
      () =
    let dir_class = if vertical then CSS.vertical else CSS.horizontal in
    let classes = fmap (fun x -> CSS.root :: dir_class :: x) classes in
    div ~a:(a_class classes :: a) (panel_a @:: return splitter @:: panel_b @:: nil ())
end

module F = Make (Tyxml.Xml) (Tyxml.Svg) (Tyxml.Html)
