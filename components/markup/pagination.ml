open Utils

module Make (Xml : Xml_sigs.NoWrap)
         (Svg : Svg_sigs.NoWrap with module Xml := Xml)
         (Html : Html_sigs.NoWrap
          with module Xml := Xml
           and module Svg := Svg) = struct

  open Html

  module CSS = struct
    include CSS

    let base = "mdc-pagination"
    let content = add_element base "content"
    let button = add_element base "button"
    let button_active = add_modifier button "active"
  end

  let create_content ?(classes = []) ?attrs content : 'a elt =
    let classes = CSS.content :: classes in
    div ~a:([a_class classes] <@> attrs) content

  let create ?(classes = []) ?attrs content =
    let classes = CSS.base :: classes in
    div ~a:([a_class classes] <@> attrs) content

end
