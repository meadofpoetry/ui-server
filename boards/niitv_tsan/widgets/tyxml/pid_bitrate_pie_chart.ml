open Components_tyxml

module CSS = struct
  let root = Util.CSS.root ^ "-pid-bitrate-pie"

  let title = BEM.add_element root "title"

  let wrapper = BEM.add_element root "wrapper"
end

module Make(Xml : Xml_sigs.NoWrap)
    (Svg : Svg_sigs.NoWrap with module Xml := Xml)
    (Html : Html_sigs.NoWrap with module Xml := Xml
                              and module Svg := Svg) = struct
  open Html

  let make_title ?(classes = []) ?(attrs = []) title =
    let classes = CSS.title :: classes in
    span ~a:([a_class classes] @ attrs) [txt title]

  let make ?(classes = []) ?(attrs = []) ~title ~canvas () =
    let classes = CSS.root :: classes in
    div ~a:([a_class classes] @ attrs)
      [ title
      ; div ~a:[a_class [CSS.wrapper]] [canvas]]
end
