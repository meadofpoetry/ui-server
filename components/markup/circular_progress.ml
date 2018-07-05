open Utils

module Make(Xml : Xml_sigs.NoWrap)
         (Svg : Svg_sigs.NoWrap with module Xml := Xml)
         (Html : Html_sigs.NoWrap
          with module Xml := Xml
           and module Svg := Svg) = struct
  open Html

  let sz                  = 50.
  let base_class          = "mdc-circular-progress"
  let circle_class        = CSS.add_element  base_class "circle"
  let indeterminate_class = CSS.add_modifier base_class "indeterminate"
  let primary_class       = CSS.add_modifier base_class "primary"
  let secondary_class     = CSS.add_modifier base_class "secondary"

  let create ?(classes=[]) ?attrs ?(indeterminate=true) ?(thickness=3.6) ?(size=40) () =
    let style = Printf.sprintf "width: %dpx; height: %dpx" size size in
    div ~a:([ a_class (classes
                       |> cons_if indeterminate indeterminate_class
                       |> List.cons base_class)
            ; a_style style
            ; a_role ["progressbar"]] <@> attrs)
      [ svg ~a:([ Svg.a_class [] ; Svg.a_viewBox (0.,0.,sz,sz)])
          [ Svg.circle ~a:[ Svg.a_class [circle_class]
                          ; Svg.a_cx (sz /. 2., None)
                          ; Svg.a_cy (sz /. 2., None)
                          ; Svg.a_fill `None
                          ; Svg.a_stroke_width (thickness, None)
                          ; Svg.a_r ((sz /. 2.) -. 5., None)] []]]


end
