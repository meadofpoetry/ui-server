open Utils

module Make(Xml : Xml_sigs.NoWrap)
         (Svg : Svg_sigs.NoWrap with module Xml := Xml)
         (Html : Html_sigs.NoWrap
          with module Xml := Xml
           and module Svg := Svg) = struct
  open Html

  let base_class              = "mdc-dynamic-grid"
  let with_overlay_grid_class = CSS.add_modifier base_class "with-overlay-grid"

  module Overlay_grid = struct

    let _class = CSS.add_element base_class "overlay-grid"

    let create ?(classes=[]) ?attrs () =
      canvas ~a:([ a_class (_class :: classes) ] <@> attrs)
        []
  end

  module Item = struct

    let _class              = CSS.add_element base_class "item"
    let ghost_class         = CSS.add_element _class "ghost"
    let resize_class        = CSS.add_element _class "resize"
    let selected_class      = CSS.add_modifier _class "selected"
    let dragging_class      = CSS.add_modifier _class "dragging"
    let drag_handle_class   = CSS.add_element _class "drag-handle"
    let select_handle_class = CSS.add_element _class "select-handle"

    let create_ghost ?(classes=[]) ?attrs () =
      div ~a:([ a_class (ghost_class :: classes)] <@> attrs) []

    let create_resize_button ?(classes=[]) ?attrs () =
      span ~a:([ a_class (resize_class :: classes)] <@> attrs) []

    let create ?(classes=[]) ?attrs ?resize_button () =
      div ~a:([ a_class (_class :: classes) ] <@> attrs) (cons_option resize_button [])
  end

  let create ?(classes=[]) ?attrs ~items () =
    div ~a:([ a_class (base_class :: classes) ] <@> attrs) items

end
