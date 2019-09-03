module CSS = struct
  let root = "mdc-radio"
  let native_control = BEM.add_element root "native-control"
  let background = BEM.add_element root "background"
  let outer_circle = BEM.add_element root "outer-circle"
  let inner_circle = BEM.add_element root "inner-circle"
  let disabled = BEM.add_modifier root "disabled"
end

module Make(Xml : Xml_sigs.NoWrap)
         (Svg : Svg_sigs.NoWrap with module Xml := Xml)
         (Html : Html_sigs.NoWrap
          with module Xml := Xml
           and module Svg := Svg) = struct
  open Html
  open Utils

  let create ?(classes = []) ?(attrs = []) ?input_id
        ?(checked = false) ?(disabled = false) ?name () : 'a elt =
    let classes =
      classes
      |> cons_if disabled CSS.disabled
      |> List.cons CSS.root in
    div ~a:([a_class classes] @ attrs)
      [ input ~a:([ a_class [CSS.native_control]
                  ; a_input_type `Radio ]
                  |> map_cons_option a_name name
                  |> cons_if_lazy checked a_checked
                  |> cons_if_lazy disabled a_disabled
                  |> map_cons_option a_id input_id)
          ()
      ; div ~a:[a_class [CSS.background]]
          [ div ~a:[a_class [CSS.outer_circle]] []
          ; div ~a:[a_class [CSS.inner_circle]] []
          ]
      ]
end
