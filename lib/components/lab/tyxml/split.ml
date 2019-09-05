module CSS = struct
  open Components_tyxml

  let root = "mdc-split"

  let horizontal = BEM.add_modifier root "horizontal"

  let vertical = BEM.add_modifier root "vertical"

  let panel = BEM.add_element root "panel"

  let splitter = BEM.add_element root "splitter"

  let splitter_icon = BEM.add_element root "splitter-icon"
end

module Make(Xml : Xml_sigs.NoWrap)
         (Svg : Svg_sigs.NoWrap with module Xml := Xml)
         (Html : Html_sigs.NoWrap
          with module Xml := Xml
           and module Svg := Svg) = struct
  open Html

  let create_panel ?(classes = []) ?(attrs = []) content () : 'a elt =
    let classes = CSS.panel :: classes in
    div ~a:([a_class classes] @ attrs) content

  let create_splitter ?(classes = []) ?(attrs = []) () : 'a elt =
    let classes = CSS.splitter :: classes in
    div ~a:([a_class classes] @ attrs)
      [div ~a:[a_class [CSS.splitter_icon]] [div []; div []]]

  let create ?(classes = []) ?(attrs = []) ?(vertical = false)
        ?(splitter = create_splitter ()) panel1 panel2 () : 'a elt =
    let dir_class = if vertical then CSS.vertical else CSS.horizontal in
    let classes = CSS.root :: dir_class :: classes in
    div ~a:([a_class classes] @ attrs)
      [panel1; splitter; panel2]
end
