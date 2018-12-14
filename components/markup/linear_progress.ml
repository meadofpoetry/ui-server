open Utils

module Make (Xml : Xml_sigs.NoWrap)
         (Svg : Svg_sigs.NoWrap with module Xml := Xml)
         (Html : Html_sigs.NoWrap
          with module Xml := Xml
           and module Svg := Svg) = struct
  open Html

  let base_class = "mdc-linear-progress"
  let buffering_dots_class = CSS.add_element base_class "buffering-dots"
  let buffer_class = CSS.add_element base_class "buffer"
  let bar_class = CSS.add_element base_class "bar"
  let primary_bar_class = CSS.add_element base_class "primary-bar"
  let secondary_bar_class = CSS.add_element base_class "secondary-bar"
  let inner_bar_class = CSS.add_element base_class "bar-inner"

  let create ?(classes = []) ?attrs
        ?(buffering_dots_classes = []) ?buffering_dots_attrs
        ?(buffer_classes = []) ?buffer_attrs
        ?(primary_bar_classes = []) ?primary_bar_attrs
        ?(secondary_bar_classes = []) ?secondary_bar_attrs
        ?(indeterminate = false) ?(reversed = false)
        ?(accent = false) () : 'a elt =
    let classes =
      classes
      |> cons_if indeterminate @@ CSS.add_modifier base_class "indeterminate"
      |> cons_if reversed @@ CSS.add_modifier base_class "reversed"
      |> cons_if accent @@ CSS.add_modifier base_class "accent"
      |> List.cons base_class in
    div ~a:([a_role ["progressbar"]; a_class classes] <@> attrs)
      [ div ~a:([a_class (buffering_dots_class :: buffering_dots_classes)]
                <@>buffering_dots_attrs) []
      ; div ~a:([a_class (buffer_class :: buffer_classes)]
                <@> buffer_attrs) []
      ; div ~a:([a_class (bar_class :: primary_bar_class :: primary_bar_classes)]
                <@> primary_bar_attrs) [ span ~a:[ a_class [inner_bar_class]] [] ]
      ; div ~a:([a_class (bar_class :: secondary_bar_class :: secondary_bar_classes)]
                <@> secondary_bar_attrs)
          [span ~a:[a_class [inner_bar_class]] []]
      ]

end
