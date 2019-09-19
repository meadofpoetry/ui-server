let string_of_float (x : float) : string = Printf.sprintf "%g" x

module CSS = struct
  let root = "mdc-circular-progress"

  let circle = BEM.add_element root "circle"

  let indeterminate = BEM.add_modifier root "indeterminate"
end

let sz = 50.

module Make
    (Xml : Xml_sigs.T)
    (Svg : Svg_sigs.T with module Xml := Xml)
    (Html : Html_sigs.T with module Xml := Xml and module Svg := Svg) =
struct
  open Html
  module CSS = CSS

  let circular_progress
      ?(classes = [])
      ?(a = [])
      ?(min = 0.)
      ?(max = 1.)
      ?(value = 0.)
      ?(indeterminate = true)
      ?(thickness = 3.6)
      ?(size = 40)
      () : 'a elt =
    let open Xml.W in
    let classes =
      classes |> Utils.cons_if indeterminate CSS.indeterminate |> List.cons CSS.root
    in
    let style = Printf.sprintf "width: %dpx; height: %dpx" size size in
    let circle =
      return
      @@ Svg.circle
           ~a:
             [ Svg.a_class (return [CSS.circle])
             ; Svg.a_cx (return (sz /. 2., None))
             ; Svg.a_cy (return (sz /. 2., None))
             ; Svg.a_fill (return `None)
             ; Svg.a_stroke_width (return (thickness, None))
             ; Svg.a_r (return ((sz /. 2.) -. 5., None)) ]
           (nil ())
    in
    let svg =
      return @@ svg ~a:[Svg.a_viewBox (return (0., 0., sz, sz))] (singleton circle)
    in
    div
      ~a:
        ([ a_class (return classes)
         ; a_style (return style)
         ; a_role (return ["progressbar"])
         ; a_aria "valuenow" (return [string_of_float value])
         ; a_aria "valuemin" (return [string_of_float min])
         ; a_aria "valuemax" (return [string_of_float max]) ]
        @ a)
      (singleton svg)
end

module F = Make (Tyxml.Xml) (Tyxml.Svg) (Tyxml.Html)
