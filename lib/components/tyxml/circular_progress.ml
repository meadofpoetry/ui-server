let string_of_float (x : float) : string = Printf.sprintf "%g" x

module CSS = struct
  let root = "mdc-circular-progress"

  let circle = BEM.add_element root "circle"

  let indeterminate = BEM.add_modifier root "indeterminate"
end

let sz = 50.

module Make
    (Xml : Xml_sigs.T with type ('a, 'b) W.ft = 'a -> 'b)
    (Svg : Svg_sigs.T with module Xml := Xml)
    (Html : Html_sigs.T with module Xml := Xml and module Svg := Svg) =
struct
  open Xml.W
  open Html

  let ( % ) f g x = f (g x)

  let circular_progress
      ?(classes = return [])
      ?(a = [])
      ?(min = return 0.)
      ?(max = return 1.)
      ?(value = return 0.)
      ?(indeterminate = true)
      ?(thickness = return 3.6)
      ?(size = return 40)
      () : 'a elt =
    let open Xml.W in
    let classes =
      fmap (Utils.cons_if indeterminate CSS.indeterminate % List.cons CSS.root) classes
    in
    let style = fmap (fun x -> Printf.sprintf "width: %dpx; height: %dpx" x x) size in
    let circle =
      return
      @@ Svg.circle
           ~a:
             [ Svg.a_class (return [CSS.circle])
             ; Svg.a_cx (return (sz /. 2., None))
             ; Svg.a_cy (return (sz /. 2., None))
             ; Svg.a_fill (return `None)
             ; Svg.a_stroke_width (fmap (fun x -> x, None) thickness)
             ; Svg.a_r (return ((sz /. 2.) -. 5., None)) ]
           (nil ())
    in
    let svg =
      return @@ svg ~a:[Svg.a_viewBox (return (0., 0., sz, sz))] (singleton circle)
    in
    div
      ~a:
        (a_class classes
        :: a_style style
        :: a_role (return ["progressbar"])
        :: a_aria "valuenow" (fmap (fun x -> [string_of_float x]) value)
        :: a_aria "valuemin" (fmap (fun x -> [string_of_float x]) min)
        :: a_aria "valuemax" (fmap (fun x -> [string_of_float x]) max)
        :: a)
      (singleton svg)
end

module F = Make (Tyxml.Xml) (Tyxml.Svg) (Tyxml.Html)
