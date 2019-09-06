module CSS = struct
  let root = "mdc-menu-surface"

  let animating_open = BEM.add_modifier root "animating-open"

  let open_ = BEM.add_modifier root "open"

  let animating_closed = BEM.add_modifier root "animating-closed"

  let anchor = BEM.add_modifier root "anchor"

  let fixed = BEM.add_modifier root "fixed"
end

module Make
    (Xml : Xml_sigs.NoWrap)
    (Svg : Svg_sigs.NoWrap with module Xml := Xml)
    (Html : Html_sigs.NoWrap with module Xml := Xml and module Svg := Svg) =
struct
  open Utils

  let create ?(classes = []) ?(attrs = []) ?(fixed = false) ?(open_ = false) content () :
      'a Html.elt =
    let classes =
      classes |> cons_if fixed CSS.fixed |> cons_if open_ CSS.open_ |> List.cons CSS.root
    in
    Html.div ~a:([Html.a_class classes] @ attrs) content
end
