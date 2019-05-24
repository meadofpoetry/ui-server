module CSS = struct
  open Components_tyxml
  let root = "mdc-dashboard"
  let edit_button = BEM.add_element root "edit-button"
  let non_editable = BEM.add_modifier root "non-editable"

  (* FIXME *)
  module Add_item = struct
    let root = BEM.add_element root "add-item"
    let thumbnail = BEM.add_element root "thumbnail"
    let thumbnail_icon = BEM.add_modifier thumbnail "icon"
    let text_box = BEM.add_element root "text-box"
    let title = BEM.add_element root "title"
    let description = BEM.add_element root "description"
    let dragging = BEM.add_modifier root "dragging"
  end

  (* FIXME *)
  module Panel = struct
    let root = BEM.add_element root "panel"
    let title = BEM.add_element root "title"
  end
end

module Item = Item

module Make(Xml : Xml_sigs.NoWrap)
         (Svg : Svg_sigs.NoWrap with module Xml := Xml)
         (Html : Html_sigs.NoWrap
          with module Xml := Xml
           and module Svg := Svg) = struct
  open Html

end
