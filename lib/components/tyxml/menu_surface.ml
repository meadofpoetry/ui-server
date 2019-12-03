module CSS = struct
  let root = "mdc-menu-surface"

  let animating_open = BEM.add_modifier root "animating-open"

  let open_ = BEM.add_modifier root "open"

  let animating_closed = BEM.add_modifier root "animating-closed"

  let anchor = BEM.add_modifier root "anchor"

  let fixed = BEM.add_modifier root "fixed"
end

module Make
    (Xml : Xml_sigs.T with type ('a, 'b) W.ft = 'a -> 'b)
    (Svg : Svg_sigs.T with module Xml := Xml)
    (Html : Html_sigs.T with module Xml := Xml and module Svg := Svg) =
struct
  open Xml.W
  open Html

  let ( % ) f g x = f (g x)

  let menu_surface
      ?(classes = return [])
      ?(a = [])
      ?(fixed = false)
      ?(open_ = false)
      ?(children = nil ())
      () =
    let classes =
      fmap
        (Utils.cons_if fixed CSS.fixed
        % Utils.cons_if open_ CSS.open_
        % List.cons CSS.root)
        classes
    in
    div ~a:(Html.a_class classes :: a) children
end

module F = Make (Tyxml.Xml) (Tyxml.Svg) (Tyxml.Html)
