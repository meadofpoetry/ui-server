let string_of_float (x : float) : string = Printf.sprintf "%g" x

module CSS = struct
  let root = "mdc-circular-progress"

  let circle = BEM.add_element root "circle"

  let indeterminate = BEM.add_modifier root "indeterminate"
end

module Make
    (Xml : Xml_sigs.NoWrap)
    (Svg : Svg_sigs.NoWrap with module Xml := Xml)
    (Html : Html_sigs.NoWrap with module Xml := Xml and module Svg := Svg) =
struct
  open Html
  open Utils

  let sz = 50.

  let create
      ?(classes = [])
      ?(attrs = [])
      ?(min = 0.)
      ?(max = 1.)
      ?(value = 0.)
      ?(indeterminate = true)
      ?(thickness = 3.6)
      ?(size = 40)
      () : 'a elt =
    let classes =
      classes |> cons_if indeterminate CSS.indeterminate |> List.cons CSS.root
    in
    let style = Printf.sprintf "width: %dpx; height: %dpx" size size in
    div
      ~a:
        ([ a_class classes
         ; a_style style
         ; a_role ["progressbar"]
         ; a_aria "valuenow" [string_of_float value]
         ; a_aria "valuemin" [string_of_float min]
         ; a_aria "valuemax" [string_of_float max] ]
        @ attrs)
      [ svg
          ~a:[Svg.a_class []; Svg.a_viewBox (0., 0., sz, sz)]
          [ Svg.circle
              ~a:
                [ Svg.a_class [CSS.circle]
                ; Svg.a_cx (sz /. 2., None)
                ; Svg.a_cy (sz /. 2., None)
                ; Svg.a_fill `None
                ; Svg.a_stroke_width (thickness, None)
                ; Svg.a_r ((sz /. 2.) -. 5., None) ]
              [] ] ]
end
