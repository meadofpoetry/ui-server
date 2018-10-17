open Utils

module Make
         (Xml : Xml_sigs.NoWrap)
         (Svg : Svg_sigs.NoWrap with module Xml := Xml)
         (Html : Html_sigs.NoWrap
          with module Xml := Xml
           and module Svg := Svg) = struct
  open Html

  let base_class = "mdc-tab"
  let content_class = CSS.add_element base_class "content"
  let icon_class = CSS.add_element base_class "icon"
  let text_label_class = CSS.add_element base_class "text-label"
  let ripple_class = CSS.add_element base_class "ripple"
  let active_class = CSS.add_modifier base_class "active"
  let stacked_class = CSS.add_modifier base_class "stacked"
  let min_width_class = CSS.add_modifier base_class "min-width"

  let create_text_label ?(classes = []) ?attrs text () =
    span ~a:([ a_class (text_label_class :: classes) ] <@> attrs)
      [pcdata text]

  let create_content ?(classes = []) ?attrs content () =
    span ~a:([a_class (content_class :: classes)] <@> attrs)
      content

  let create ?(classes = []) ?attrs ?(active = false) ?(stacked = false)
        ?(min_width = false) content indicator () =
    button ~a:([ a_class (classes
                          |> cons_if active active_class
                          |> cons_if stacked stacked_class
                          |> cons_if min_width min_width_class
                          |> List.cons base_class)
               ; a_role ["tab"]]
               <@> attrs)
      [ content
      ; indicator
      ; span ~a:[a_class [ripple_class]] []
      ]

end
