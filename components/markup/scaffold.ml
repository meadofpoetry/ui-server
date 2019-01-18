open Utils

module Make(Xml : Xml_sigs.NoWrap)
         (Svg : Svg_sigs.NoWrap with module Xml := Xml)
         (Html : Html_sigs.NoWrap
          with module Xml := Xml
           and module Svg := Svg) = struct
  open Html

  module CSS = struct
    include CSS
    let root = "mdc-scaffold"
    let drawer_frame = add_element root "drawer-frame"
    let app_content = add_element root "app-content"

    let drawer_frame_inner = add_modifier drawer_frame "inner"
    let drawer_frame_outer = add_modifier drawer_frame "outer"
  end

  let create ?(classes = []) ?attrs content () =
    let classes = CSS.root :: classes in
    div ~a:([a_class classes] <@> attrs) content

end
