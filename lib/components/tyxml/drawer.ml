module CSS = struct
  include Side_sheet.Make_css (struct
    let root = "mdc-drawer"
  end)

  (** Non-scrollable element that exists at the top of the drawer. *)
  let header = BEM.add_element root "header"

  (** Title text element of the drawer. *)
  let title = BEM.add_element root "title"

  (** Subtitle text element of the drawer. *)
  let subtitle = BEM.add_element root "subtitle"
end

module Make
    (Xml : Xml_sigs.T with type ('a, 'b) W.ft = 'a -> 'b)
    (Svg : Svg_sigs.T with module Xml := Xml)
    (Html : Html_sigs.T with module Xml := Xml and module Svg := Svg) =
struct
  open Xml.W
  open Html

  let drawer_scrim ?(classes = return []) ?(a = []) ?(children = nil ()) () : 'a elt =
    let classes = fmap (List.cons CSS.scrim) classes in
    div ~a:(a_class classes :: a) children

  let drawer_title
      ?(tag = h3)
      ?(classes = return [])
      ?(a = [])
      ?title
      ?(children = nil ())
      () =
    let classes = fmap (List.cons CSS.title) classes in
    let children =
      match title with
      | None -> children
      | Some x -> cons (return (txt x)) children
    in
    tag ~a:(a_class classes :: a) children

  let drawer_subtitle
      ?(tag = h6)
      ?(classes = return [])
      ?(a = [])
      ?subtitle
      ?(children = nil ())
      () =
    let classes = fmap (List.cons CSS.subtitle) classes in
    let children =
      match subtitle with
      | None -> children
      | Some x -> cons (return (txt x)) children
    in
    tag ~a:(a_class classes :: a) children

  let drawer_header ?(classes = return []) ?(a = []) ?(children = nil ()) () : 'a elt =
    let classes = fmap (List.cons CSS.header) classes in
    div ~a:(a_class classes :: a) children

  let drawer_content ?(classes = return []) ?(a = []) ?(children = nil ()) () : 'a elt =
    let classes = fmap (List.cons CSS.content) classes in
    div ~a:(a_class classes :: a) children

  let drawer ?(classes = return []) ?(a = []) ?(children = nil ()) () =
    let classes = fmap (List.cons CSS.root) classes in
    aside ~a:(a_class classes :: a) children
end

module F = Make (Tyxml.Xml) (Tyxml.Svg) (Tyxml.Html)
