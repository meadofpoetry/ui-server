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
  | `Error ]

module Make
    (Xml : Xml_sigs.NoWrap)
    (Svg : Svg_sigs.NoWrap with module Xml := Xml)
    (Html : Html_sigs.NoWrap with module Xml := Xml and module Svg := Svg) =
struct
  open Html
  module Typography = Typography.Make (Xml) (Svg) (Html)

  let create_dots ?(classes = []) ?(attrs = []) () =
    let classes = CSS.dots :: classes in
    span ~a:([a_class classes] @ attrs) [span [txt "."]; span [txt "."]; span [txt "."]]

  let create_text ?(classes = []) ?(attrs = []) ?(loading = false) ?text () =
    let classes =
      classes |> Utils.cons_if loading CSS.text_loading |> List.cons CSS.text
    in
    let content = if loading then [create_dots ()] else [] in
    let text =
      match text with
      | None -> None
      | Some (`Text s) -> Some (txt s)
      | Some (`Element e) -> Some e
    in
    span ~a:([a_class classes] @ attrs) Utils.(text ^:: content)

  let create_content ?(classes = []) ?(attrs = []) ?loading ?icon ?text ?children () =
    let classes = CSS.content :: classes in
    let children =
      match children with
      | Some x -> x
      | None ->
          let text =
            match text with
            | None -> None
            | Some (`Text s) -> Some (create_text ?loading ~text:(`Text s) ())
            | Some (`Element e) -> Some e
          in
          Utils.(icon ^:: text ^:: [])
    in
    div ~a:([a_class classes] @ attrs) children

  let create
      ?(classes = [])
      ?(attrs = [])
      ?(error = false)
      ?loading
      ?icon
      ?text
      ?children
      () =
    let classes = classes |> Utils.cons_if error CSS.error |> List.cons CSS.root in
    let children =
      match children with
      | Some x -> x
      | None -> [create_content ?loading ?icon ?text ()]
    in
    div ~a:([a_class classes] @ attrs) children
end

module Markup = Make (Tyxml.Xml) (Tyxml.Svg) (Tyxml.Html)
