open Utils

module Make(Xml : Xml_sigs.NoWrap)
         (Svg : Svg_sigs.NoWrap with module Xml := Xml)
         (Html : Html_sigs.NoWrap
          with module Xml := Xml
           and module Svg := Svg) = struct
  open Html

  let base_class           = "mdc-snackbar"
  let text_class           = CSS.add_element base_class "text"
  let action_wrapper_class = CSS.add_element base_class "action-wrapper"
  let action_button_class  = CSS.add_element base_class "action-button"
  let align_start_class    = CSS.add_modifier base_class "align-start"

  let create ?(classes=[]) ?attrs ?(start_aligned=false) () =
    div ~a:([ a_class (classes
                       |> cons_if start_aligned align_start_class
                       |> List.cons base_class)
            ; a_aria "live" ["assertive"]
            ; a_aria "atomic" ["true"]
            ; a_aria "hidden" ["true"]]
            <@> attrs)
      [ div ~a:([a_class [text_class]]) []
      ; div ~a:([a_class [action_wrapper_class]])
          [ button ~a:[ a_class [action_button_class]
                      ; a_button_type `Button ]
              [] ]
      ]

end
