(** Inspired by <https://glidejs.com> *)

open Components_tyxml

module CSS = struct
  let root = "mdc-glide"

  let track = BEM.add_element root "track"

  let slide = BEM.add_element root "slide"
end

module Make
    (Xml : Xml_sigs.T with type ('a, 'b) W.ft = 'a -> 'b)
    (Svg : Svg_sigs.T with module Xml := Xml)
    (Html : Html_sigs.T with module Xml := Xml and module Svg := Svg) =
struct
  open Xml.W
  open Html

  let glide_slide ?(classes = return []) ?(a = []) ?(children = nil ()) () =
    let classes = fmap (fun x -> CSS.slide :: x) classes in
    li ~a:(a_class classes :: a) children

  let glide_track ?(classes = return []) ?(a = []) ?(children = nil ()) () =
    let classes = fmap (fun x -> CSS.track :: x) classes in
    ul ~a:(a_class classes :: a) children

  let glide ?(classes = return []) ?(a = []) ?slides ?children () =
    let classes = fmap (fun x -> CSS.root :: x) classes in
    let children =
      match children with
      | Some x -> x
      | None ->
          let slides = Option.value ~default:(nil ()) slides in
          singleton (return (glide_track ~children:slides ()))
    in
    div ~a:(a_class classes :: a) children
end

module F = Make (Tyxml.Xml) (Tyxml.Svg) (Tyxml.Html)
