open Utils

module Make(Xml : Xml_sigs.NoWrap)
         (Svg : Svg_sigs.NoWrap with module Xml := Xml)
         (Html : Html_sigs.NoWrap
          with module Xml := Xml
           and module Svg := Svg) = struct
  open Html

  let base_class        = "mdc-dashboard"
  let edit_button_class = CSS.add_element base_class "edit-button"

  module Item = struct
    let _class        = CSS.add_element base_class "item"
    let content_class = CSS.add_element _class "content"
    let heading_class = CSS.add_element _class "heading"
    let buttons_class = CSS.add_element _class "heading-buttons"
    let button_class  = CSS.add_element _class "heading-button"
    let editing_class = CSS.add_modifier _class "editing"
  end

  module Add_item = struct
    let _class               = CSS.add_element base_class "add-item"
    let thumbnail_class      = CSS.add_element _class "thumbnail"
    let thumbnail_icon_class = CSS.add_modifier thumbnail_class "icon"
    let text_box_class       = CSS.add_element _class "text-box"
    let title_class          = CSS.add_element _class "title"
    let description_class    = CSS.add_element _class "description"
    let dragging_class       = CSS.add_modifier _class "dragging"
  end

  module Panel = struct
    let _class      = CSS.add_element base_class "panel"
    let title_class = CSS.add_element _class "title"
  end

end
