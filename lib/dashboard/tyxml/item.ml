module CSS = struct
  open Components_tyxml
  let root = "mdc-dashboard-item"
  let item_content = BEM.add_element root "content"
  let item_widget = BEM.add_element root "widget"
  let heading = BEM.add_element root "heading"
  let heading_title = BEM.add_element root "heading-title"
  let heading_buttons = BEM.add_element root "heading-buttons"
  let heading_button = BEM.add_element root "heading-button"
  let editing = BEM.add_modifier root "editing"
end
