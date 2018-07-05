open Utils

module Make(Xml : Xml_sigs.NoWrap)
         (Svg : Svg_sigs.NoWrap with module Xml := Xml)
         (Html : Html_sigs.NoWrap
          with module Xml := Xml
           and module Svg := Svg) = struct
  open Html

  let base_class   = "mdc-fab-speed-dial"

  let main_class    = CSS.add_element base_class "main"
  let action_class  = CSS.add_element base_class "action"
  let actions_class = CSS.add_element base_class "actions"

  let fling_class  = CSS.add_modifier base_class "animation-fling"
  let scale_class  = CSS.add_modifier base_class "animation-scale"

  let up_class     = CSS.add_modifier base_class "direction-up"
  let down_class   = CSS.add_modifier base_class "direction-down"
  let left_class   = CSS.add_modifier base_class "direction-left"
  let right_class  = CSS.add_modifier base_class "direction-right"

  let opened_class = CSS.add_modifier base_class "opened"

end
