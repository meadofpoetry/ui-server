module CSS = struct
  (** Mandatory. *)
  let root = "mdc-floating-label"

  (** Indicates the label is floating in the floating position. *)
  let float_above = BEM.add_modifier root "float-above"

  (** Shakes the label. *)
  let shake = BEM.add_modifier root "shake"
end

module Make
    (Xml : Xml_sigs.T with type ('a, 'b) W.ft = 'a -> 'b)
    (Svg : Svg_sigs.T with module Xml := Xml)
    (Html : Html_sigs.T with module Xml := Xml and module Svg := Svg) =
struct
  open Xml.W
  open Html

  let ( % ) f g x = f (g x)

  let floating_label
      ?(classes = return [])
      ?(a = [])
      ?(float_above = false)
      ?for_
      ?label
      ?(children = nil ())
      () =
    let classes =
      fmap (Utils.cons_if float_above CSS.float_above % List.cons CSS.root) classes
    in
    let children =
      match label with
      | None -> children
      | Some x -> cons (return (txt x)) children
    in
    Html.label
      ~a:(a_class classes :: a |> Utils.map_cons_option a_label_for for_)
      children
end

module F = Make (Tyxml.Xml) (Tyxml.Svg) (Tyxml.Html)
