module CSS = struct
  let root = "mdc-form-field"

  let align_end = BEM.add_modifier root "align-end"
end

module Make
    (Xml : Xml_sigs.T with type ('a, 'b) W.ft = 'a -> 'b)
    (Svg : Svg_sigs.T with module Xml := Xml)
    (Html : Html_sigs.T with module Xml := Xml and module Svg := Svg) =
struct
  open Xml.W
  open Html

  let ( % ) f g x = f (g x)

  let ( ^:: ) x l = Option.fold ~none:l ~some:(fun x -> cons x l) x

  let form_field_label
      ?(classes = return [])
      ?(a = [])
      ?for_
      ?label
      ?(children = nil ())
      () =
    let children =
      match label with
      | None -> children
      | Some x -> cons (return (txt x)) children
    in
    Html.label
      ~a:(a_class classes :: a |> Utils.map_cons_option a_label_for for_)
      children

  let form_field
      ?(classes = return [])
      ?(a = [])
      ?(align_end = false)
      ?label_for
      ?(children = nil ())
      ?input
      ?label
      () =
    let classes =
      fmap (Utils.cons_if align_end CSS.align_end % List.cons CSS.root) classes
    in
    let label =
      match label with
      | None -> None
      | Some x -> Some (return @@ form_field_label ?for_:label_for ~label:x ())
    in
    let children = input ^:: label ^:: children in
    div ~a:(a_class classes :: a) children
end

module F = Make (Tyxml.Xml) (Tyxml.Svg) (Tyxml.Html)
