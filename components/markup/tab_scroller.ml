open Utils

module Make
         (Xml : Xml_sigs.NoWrap)
         (Svg : Svg_sigs.NoWrap with module Xml := Xml)
         (Html : Html_sigs.NoWrap
          with module Xml := Xml
           and module Svg := Svg) = struct
  open Html

  let base_class = "mdc-tab-scroller"
  let scroll_test_class = CSS.add_element base_class "test"
  let scroll_area_class = CSS.add_element base_class "scroll-area"
  let scroll_area_scroll_class = CSS.add_modifier scroll_area_class "scroll"
  let scroll_content_class = CSS.add_element base_class "scroll-content"
  let align_start_class = CSS.add_modifier base_class "align-start"
  let align_end_class = CSS.add_modifier base_class "align-end"
  let align_center_class = CSS.add_modifier base_class "align-center"

  let create_scroll_content ?(classes=[]) ?attrs tabs () =
    div ~a:([ a_class (scroll_content_class :: classes) ]
            <@> attrs)
      tabs

  let create_scroll_area ?(classes=[]) ?attrs ~content () =
    div ~a:([ a_class (scroll_area_class :: classes) ]
            <@> attrs)
      [ content ]

  let create ?(classes=[]) ?attrs ~scroll_area () =
    div ~a:([ a_class (base_class :: classes) ]
            <@> attrs)
      [ scroll_area ]

end
