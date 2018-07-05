open Utils

module Make(Xml : Xml_sigs.NoWrap)
         (Svg : Svg_sigs.NoWrap with module Xml := Xml)
         (Html : Html_sigs.NoWrap
          with module Xml := Xml
           and module Svg := Svg) = struct
  open Html

  let base_class           = "mdc-radio"
  let native_control_class = CSS.add_element base_class "native-control"
  let background_class     = CSS.add_element base_class "background"
  let outer_circle_class   = CSS.add_element base_class "outer-circle"
  let inner_circle_class   = CSS.add_element base_class "inner-circle"
  let disabled_class       = CSS.add_modifier base_class "disabled"

  let create ?(classes=[]) ?attrs ?input_id ?(checked=false) ?(disabled=false) ?name () =
    div ~a:([ a_class (classes
                       |> cons_if disabled disabled_class
                       |> List.cons base_class) ] <@> attrs)
      [ input ~a:([ a_class [native_control_class]
                  ; a_input_type `Radio ]
                  |> map_cons_option a_name name
                  |> cons_if checked @@ a_checked ()
                  |> cons_if disabled @@ a_disabled ()
                  |> map_cons_option a_id input_id)
          ()
      ; div ~a:[ a_class [background_class] ]
          [ div ~a:[ a_class [outer_circle_class]] []
          ; div ~a:[ a_class [inner_circle_class]] [] ]
      ]

end
