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

type appearance = [ `Icon | `Progress | `Error ]

module Make
    (Xml : Xml_sigs.T with type ('a, 'b) W.ft = 'a -> 'b)
    (Svg : Svg_sigs.T with module Xml := Xml)
    (Html : Html_sigs.T with module Xml := Xml and module Svg := Svg) =
struct
  open Xml.W
  open Html
  module Typography = Typography.Make (Xml) (Svg) (Html)

  let ( % ) f g x = f (g x)

  let ( @:: ) = cons

  let ( ^:: ) x l = Option.fold ~none:l ~some:(fun x -> cons x l) x

  let placeholder_dots ?(classes = return []) ?(a = []) () =
    let classes = fmap (fun x -> CSS.dots :: x) classes in
    let dot () = return (span (singleton (return (txt (return "."))))) in
    span ~a:(a_class classes :: a) (dot () @:: dot () @:: dot () @:: nil ())

  let placeholder_text ?(classes = return []) ?(a = []) ?(loading = false) ?text
      () =
    let classes =
      fmap (Utils.cons_if loading CSS.text_loading % List.cons CSS.text) classes
    in
    let content =
      if loading then singleton (return (placeholder_dots ())) else nil ()
    in
    let text =
      match text with
      | None -> None
      | Some (`Text s) -> Some (return (txt s))
      | Some (`Element e) -> Some (return e)
    in
    span ~a:(a_class classes :: a) (text ^:: content)

  let placeholder_content ?(classes = return []) ?(a = []) ?loading ?icon ?text
      ?children () =
    let classes = fmap (fun x -> CSS.content :: x) classes in
    let children =
      match children with
      | Some x -> x
      | None ->
          let text =
            match text with
            | None -> None
            | Some (`Text s) ->
                Some (return (placeholder_text ?loading ~text:(`Text s) ()))
            | Some (`Element e) -> Some (return e)
          in
          icon ^:: text ^:: nil ()
    in
    div ~a:(a_class classes :: a) children

  let placeholder ?(classes = return []) ?(a = []) ?(error = false) ?loading
      ?icon ?text ?children () =
    let classes =
      fmap (Utils.cons_if error CSS.error % List.cons CSS.root) classes
    in
    let children =
      match children with
      | Some x -> x
      | None -> singleton (return (placeholder_content ?loading ?icon ?text ()))
    in
    div ~a:(a_class classes :: a) children
end

module F = Make (Tyxml.Xml) (Tyxml.Svg) (Tyxml.Html)
