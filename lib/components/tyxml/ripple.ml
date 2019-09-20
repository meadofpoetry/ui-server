module CSS = struct
  module Var = struct
    let left = "--mdc-ripple-left"

    let top = "--mdc-ripple-top"

    let fg_size = "--mdc-ripple-fg-size"

    let fg_scale = "--mdc-ripple-fg-scale"

    let fg_translate_start = "--mdc-ripple-fg-translate-start"

    let fg_translate_end = "--mdc-ripple-fg-translate-end"

    let vars = [left; top; fg_size; fg_scale; fg_translate_start; fg_translate_end]
  end

  let root = "mdc-ripple-upgraded"

  let unbounded = BEM.add_modifier root "unbounded"

  let bg_focused = BEM.add_modifier root "background-focused"

  let fg_activation = BEM.add_modifier root "foreground-activation"

  let fg_deactivation = BEM.add_modifier root "foreground-deactivation"
end

module Make
    (Xml : Xml_sigs.T with type ('a, 'b) W.ft = 'a -> 'b)
    (Svg : Svg_sigs.T with module Xml := Xml)
    (Html : Html_sigs.T with module Xml := Xml and module Svg := Svg) =
struct
  let unbounded_attr = Html.a_user_data "mdc-ripple-is-unbounded" (Xml.W.return "")
end

module F = Make (Tyxml.Xml) (Tyxml.Svg) (Tyxml.Html)
