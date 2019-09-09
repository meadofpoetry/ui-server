module CSS = struct
  let root = "mdc-divider"

  let inset = BEM.add_modifier root "inset"
end

module Make
    (Xml : Xml_sigs.NoWrap)
    (Svg : Svg_sigs.NoWrap with module Xml := Xml)
    (Html : Html_sigs.NoWrap with module Xml := Xml and module Svg := Svg) =
struct
  open Html

  let create
      ?(classes = [])
      ?(attrs = [])
      ?(inset = false)
      ~(tag : ?a:'a attrib list_wrap -> 'b) : 'b =
    let classes = classes |> Utils.cons_if inset CSS.inset |> List.cons CSS.root in
    tag ~a:([a_class classes] @ attrs)

  let create_hr = create ~tag:hr

  let create_li ?classes ?attrs ?inset () = create ~tag:li ?classes ?attrs ?inset []
end

module Markup = Make (Tyxml.Xml) (Tyxml.Svg) (Tyxml.Html)
