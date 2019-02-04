open Utils

module Make(Xml : Xml_sigs.NoWrap)
         (Svg : Svg_sigs.NoWrap with module Xml := Xml)
         (Html : Html_sigs.NoWrap
          with module Xml := Xml
           and module Svg := Svg) = struct
  open Html

  let base_class = "mdc-divider"
  let inset_class = CSS.add_modifier base_class "inset"

  let create ?(classes = []) ?attrs ?(inset = false) () : 'a elt =
    hr ~a:([a_class (classes
                     |> cons_if inset inset_class
                     |> List.cons base_class)]
           <@> attrs) ()

end
