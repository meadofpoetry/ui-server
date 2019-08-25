open Components_tyxml

module CSS = struct
  let root = "mdc-overflow-menu"
  let actions = BEM.add_element root "actions"
  let overflow = BEM.add_element root "overflow"
end

module Make(Xml : Xml_sigs.NoWrap)
    (Svg : Svg_sigs.NoWrap with module Xml := Xml)
    (Html : Html_sigs.NoWrap with module Xml := Xml
                              and module Svg := Svg) = struct
  open Html

  let create ?(classes = []) ?(attrs = []) ~overflow ~actions ~menu () : 'a elt =
    let classes = CSS.root :: classes in
    div ~a:([a_class classes] @ attrs)
      [ div ~a:[a_class [CSS.actions]] actions
      ; div ~a:[a_class [CSS.overflow]] [overflow; menu]
      ]
end
