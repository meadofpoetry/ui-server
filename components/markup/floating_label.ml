open Utils
open Containers

module Make(Xml : Xml_sigs.NoWrap)
         (Svg : Svg_sigs.NoWrap with module Xml := Xml)
         (Html : Html_sigs.NoWrap
          with module Xml := Xml
           and module Svg := Svg) = struct
  open Html

  let base_class = "mdc-line-ripple"
  let float_above_class = CSS.add_modifier base_class "float_above"
  let shake_class = CSS.add_modifier base_class "shake"

  let create ?(classes = []) ?attrs ~data ~for_ () =
    label ~a:([ a_class (base_class :: classes) ]
              |> List.cons (a_label_for for_)
              <@> attrs) [pcdata data]

end
