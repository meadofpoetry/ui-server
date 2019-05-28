module CSS = struct
  open Components_tyxml
  let root = "mdc-dynamic-grid"
  let with_overlay_grid = BEM.add_modifier root "with-overlay-grid"
  let overlay_grid = BEM.add_element root "overlay-grid"
  let item = root ^ "-item"
  let ghost = BEM.add_element root "ghost"
  let item_selected = BEM.add_modifier item "selected"
  let item_dragging = BEM.add_modifier item "dragging"
  let item_resize_handle = BEM.add_element item "resize-handle"
  let item_drag_handle = BEM.add_element item "drag-handle"
  let item_select_handle = BEM.add_element item "select-handle"
end

module Make(Xml : Xml_sigs.NoWrap)
         (Svg : Svg_sigs.NoWrap with module Xml := Xml)
         (Html : Html_sigs.NoWrap
          with module Xml := Xml
           and module Svg := Svg) = struct
  open Html
  open Components_tyxml.Utils

  let create_overlay_grid ?(classes = []) ?attrs () =
    let classes = CSS.overlay_grid :: classes in
    canvas ~a:([a_class classes] <@> attrs) []

  let create_ghost ?(classes = []) ?attrs () : 'a elt =
    let classes = CSS.ghost :: classes in
    div ~a:([a_class classes] <@> attrs) []

  let create_resize_handle ?(classes = []) ?attrs () : 'a elt =
    let classes = CSS.item_resize_handle :: classes in
    span ~a:([a_class classes] <@> attrs) []

  let create_item ?(classes = []) ?attrs ?resize_handle () : 'a elt =
    let classes = CSS.item :: classes in
    div ~a:([a_class classes] <@> attrs) (cons_option resize_handle [])

  let create ?(classes = []) ?attrs ~items () : 'a elt =
    let classes = CSS.root :: classes in
    div ~a:([a_class classes] <@> attrs) items

end
