open Utils

module Make(Xml : Xml_sigs.NoWrap)
         (Svg : Svg_sigs.NoWrap with module Xml := Xml)
         (Html : Html_sigs.NoWrap
          with module Xml := Xml
           and module Svg := Svg) = struct
  open Html

  let base_class = "mdc-split"
  let horizontal_class = CSS.add_modifier base_class "horizontal"
  let vertical_class = CSS.add_modifier base_class "vertical"

  let panel_class = CSS.add_element base_class "panel"
  let splitter_class = CSS.add_element base_class "splitter"
  let splitter_icon_class = CSS.add_element base_class "splitter-icon"

  let create_panel ?(classes = []) ?attrs content () : 'a elt =
    div ~a:([a_class (panel_class :: classes)] <@> attrs) content

  let create_splitter ?(classes = []) ?attrs () : 'a elt =
    div ~a:([ a_class (splitter_class :: classes) ] <@> attrs)
      [ div ~a:[ a_class [ splitter_icon_class ]]
          [ div []; div [] ]]

  let create ?(classes = []) ?attrs ?(vertical = false)
        ?(splitter = create_splitter ()) panel1 panel2 () : 'a elt =
    let dir_class = if vertical then vertical_class else horizontal_class in
    div ~a:([a_class (base_class :: dir_class :: classes)] <@> attrs)
      [panel1; splitter; panel2]

end
