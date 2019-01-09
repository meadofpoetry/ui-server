open Utils

module Make (Xml : Xml_sigs.NoWrap)
         (Svg : Svg_sigs.NoWrap with module Xml := Xml)
         (Html : Html_sigs.NoWrap
          with module Xml := Xml
           and module Svg := Svg) = struct

  open Html

  let base_class = "mdc-checkbox"
  let disabled_class = CSS.add_modifier base_class "disabled"
  let native_control_class = CSS.add_element base_class "native-control"
  let background_class = CSS.add_element base_class "background"
  let checkmark_class = CSS.add_element base_class "checkmark"
  let checkmark_path_class = CSS.add_element base_class "checkmark-path"
  let mixedmark_class = CSS.add_element base_class "mixedmark"

  let create ?(classes = []) ?attrs
        ?input_id ?(disabled = false) ?(checked = false) () : 'a elt =
    let (classes : string list) =
      classes
      |> cons_if disabled @@ CSS.add_modifier base_class "disabled"
      |> List.cons base_class in
    div ~a:([a_class classes] <@> attrs)
      [ input ~a:([ a_input_type `Checkbox
                  ; a_class [native_control_class]]
                  |> map_cons_option a_id input_id
                  |> cons_if disabled @@ a_disabled ()
                  |> cons_if checked @@ a_checked ()) ()
      ; div ~a:[a_class [background_class]]
          [ svg ~a:([ Svg.a_class [checkmark_class]
                    ; Svg.a_viewBox (0.0, 0.0, 24.0, 24.0)])
              [Svg.path ~a:([ Svg.a_class [checkmark_path_class]
                            ; Svg.a_fill `None
                            ; Svg.a_stroke (`Color ("white", None))
                            ; Svg.a_d "M1.73,12.91 8.1,19.28 22.79,4.59"])
                 []]
          ; div ~a:[a_class [mixedmark_class]] []]]

end
