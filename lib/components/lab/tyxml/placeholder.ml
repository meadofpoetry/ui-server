open Components_tyxml

module CSS = struct
  let root = "mdc-placeholder"

  let content = BEM.add_element root "content"

  let dots = BEM.add_element root "dots"

  let text = BEM.add_element root "text"

  let text_loading = BEM.add_modifier text "loading"

  let error = BEM.add_modifier root "error"
end

let error_svg_path = Components_tyxml.Svg_icons.alert_decagram

type appearance =
  [ `Icon
  | `Progress
  | `Error
  ]

module Make(Xml : Xml_sigs.NoWrap)
    (Svg : Svg_sigs.NoWrap with module Xml := Xml)
    (Html : Html_sigs.NoWrap with module Xml := Xml
                              and module Svg := Svg) = struct
  open Html

  module Typography = Typography.Make(Xml)(Svg)(Html)

  let make_dots ?(classes = []) ?(attrs = []) () =
    let classes = CSS.dots :: classes in
    span ~a:([a_class classes] @ attrs)
      [ span [txt "."]
      ; span [txt "."]
      ; span [txt "."]
      ]

  let make_text ?(classes = []) ?(attrs = []) ?(loading = false) text =
    let classes =
      classes
      |> Utils.cons_if loading CSS.text_loading
      |> List.cons CSS.text in
    let content = if loading then [make_dots ()] else [] in
    span ~a:([a_class classes] @ attrs) (text :: content)

  let make_text_string ?classes ?attrs ?loading text =
    make_text ?classes ?attrs ?loading (txt text)

  let make_content ?(classes = []) ?(attrs = []) content =
    let classes = CSS.content :: classes in
    div ~a:([a_class classes] @ attrs) content

  let make ?(classes = []) ?(attrs = []) ?(error = false) ~content () =
    let classes =
      classes
      |> Utils.cons_if error CSS.error
      |> List.cons CSS.root in
    div ~a:([a_class classes] @ attrs) [content]

  let make_simple ?classes ?attrs ?error ?loading widget text =
    let content = make_content [widget; make_text_string ?loading text] in
    make ?classes ?attrs ?error ~content ()
end
