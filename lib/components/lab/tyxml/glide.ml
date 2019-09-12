(** Inspired by <https://glidejs.com> *)

open Components_tyxml

module CSS = struct
  let root = "mdc-glide"

  let track = BEM.add_element root "track"

  let slide = BEM.add_element root "slide"
end

module Make
    (Xml : Xml_sigs.NoWrap)
    (Svg : Svg_sigs.NoWrap with module Xml := Xml)
    (Html : Html_sigs.NoWrap with module Xml := Xml and module Svg := Svg) =
struct
  open Html

  let create_slide ?(classes = []) ?(attrs = []) ?(children = []) () =
    let classes = CSS.slide :: classes in
    li ~a:([a_class classes] @ attrs) children

  let create_track ?(classes = []) ?(attrs = []) ?(children = []) () =
    let classes = CSS.track :: classes in
    ul ~a:([a_class classes] @ attrs) children

  let create ?(classes = []) ?(attrs = []) ?slides ?children () =
    let classes = CSS.root :: classes in
    let children =
      match children with
      | Some x -> x
      | None ->
          let slides = Option.value ~default:[] slides in
          [create_track ~children:slides ()]
    in
    div ~a:([a_class classes] @ attrs) children
end
