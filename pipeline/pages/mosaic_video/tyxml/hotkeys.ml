open Components_tyxml

module CSS = struct
  let root = "hotkeys"

  let section = BEM.add_element root "section"

  let section_title = BEM.add_element root "section-title"

  let option = BEM.add_element root "option"

  let label = BEM.add_element root "label"

  let hotkey = BEM.add_element root "hotkey"
end

module Make
    (Xml : Xml_sigs.NoWrap)
    (Svg : Svg_sigs.NoWrap with module Xml := Xml)
    (Html : Html_sigs.NoWrap with module Xml := Xml and module Svg := Svg) =
struct
  open Html

  let create_hotkey ?(classes = []) ?(a = []) hotkey () : 'a elt =
    let classes = CSS.hotkey :: classes in
    div ~a:(a_class classes :: a) [ txt hotkey ]

  let create_label ?(classes = []) ?(a = []) label () : 'a elt =
    let classes = CSS.label :: classes in
    div ~a:(a_class classes :: a) [ txt label ]

  let create_option ?(classes = []) ?(a = []) ~label ~hotkey () : 'a elt =
    let classes = CSS.option :: classes in
    div ~a:(a_class classes :: a) [ label; hotkey ]

  let create_section_title ?(classes = []) ?(a = []) title () : 'a elt =
    let classes = CSS.section_title :: classes in
    div ~a:(a_class classes :: a) [ txt title ]

  let create_section ?(classes = []) ?(a = []) ~title ~options () : 'a elt =
    let classes = CSS.section :: classes in
    div ~a:(a_class classes :: a) [ title; div options ]

  let create ?(classes = []) ?(a = []) ~sections () : 'a elt =
    let classes = CSS.root :: classes in
    div ~a:(a_class classes :: a) sections
end
