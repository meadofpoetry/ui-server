
module CSS = struct
  let root = "mdc-pagination"
  let content = BEM.add_element root "content"
  let button = BEM.add_element root "button"
  let button_active = BEM.add_modifier button "active"
end

module Make (Xml : Xml_sigs.NoWrap)
         (Svg : Svg_sigs.NoWrap with module Xml := Xml)
         (Html : Html_sigs.NoWrap
          with module Xml := Xml
           and module Svg := Svg) = struct
  open Html
      
  let create_content ?(classes = []) ?(attrs = []) content : 'a elt =
    let classes = CSS.content :: classes in
    div ~a:([a_class classes] @ attrs) content

  let create ?(classes = []) ?(attrs = []) content =
    let classes = CSS.root :: classes in
    div ~a:([a_class classes] @ attrs) content
end
