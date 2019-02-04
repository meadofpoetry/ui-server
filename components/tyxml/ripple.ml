open Utils

module Make(Xml : Xml_sigs.NoWrap)
         (Svg : Svg_sigs.NoWrap with module Xml := Xml)
         (Html : Html_sigs.NoWrap
          with module Xml := Xml
           and module Svg := Svg) = struct
  open Html

  let base_class = "mdc-ripple-upgraded"
  let unbounded_class = CSS.add_modifier base_class "unbounded"
  let bg_focused_class = CSS.add_modifier base_class "background-focused"
  let fg_activation_class = CSS.add_modifier base_class "foreground-activation"
  let fg_deactivation_class = CSS.add_modifier base_class "foreground-deactivation"

  let var_left = "--mdc-ripple-left"
  let var_top = "--mdc-ripple-top"
  let var_fg_size = "--mdc-ripple-fg-size"
  let var_fg_scale = "--mdc-ripple-fg-scale"
  let var_fg_translate_start = "--mdc-ripple-fg-translate-start"
  let var_fg_translate_end = "--mdc-ripple-fg-translate-end"

  let vars =
    [ var_left
    ; var_top
    ; var_fg_size
    ; var_fg_scale
    ; var_fg_translate_start
    ; var_fg_translate_end
    ]

  let unbounded_attr = a_user_data "mdc-ripple-is-unbounded" ""

end
