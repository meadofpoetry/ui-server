open Utils

module Make
         (Xml : Xml_sigs.NoWrap)
         (Svg : Svg_sigs.NoWrap with module Xml := Xml)
         (Html : Html_sigs.NoWrap
          with module Xml := Xml
           and module Svg := Svg) = struct
  open Html

  let base_class           = "mdc-switch"
  let native_control_class = CSS.add_element base_class "native-control"
  let background_class     = CSS.add_element base_class "background"
  let knob_class           = CSS.add_element base_class "knob"
  let disabled_class       = CSS.add_modifier base_class "disabled"

  let create ?input_id ?(classes=[]) ?attrs ?(disabled=false) () =
    div ~a:([ a_class (classes
                       |> cons_if disabled disabled_class
                       |> List.cons base_class) ] <@> attrs)
      [ input ~a:([ a_input_type `Checkbox
                  ; a_class [native_control_class]]
                  |> cons_if disabled @@ a_disabled ()
                  |> map_cons_option a_id input_id) ()
      ; div ~a:([ a_class [background_class]]) [ div ~a:([ a_class [knob_class]]) []] ]

end
