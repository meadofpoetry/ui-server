module CSS = struct
  (** Mandatory. *)
  let root = "mdc-floating-label"

  (** Indicates the label is floating in the floating position. *)
  let float_above = BEM.add_modifier root "float-above"

  (** Shakes the label. *)
  let shake = BEM.add_modifier root "shake"
end

module Make
    (Xml : Xml_sigs.NoWrap)
    (Svg : Svg_sigs.NoWrap with module Xml := Xml)
    (Html : Html_sigs.NoWrap with module Xml := Xml and module Svg := Svg) =
struct
  open Html

  let create ?(classes = []) ?(attrs = []) ?for_ ?label ?(children = []) () : 'a elt =
    let classes = CSS.root :: classes in
    Html.label
      ~a:([a_class classes] @ attrs |> Utils.map_cons_option a_label_for for_)
      (Utils.map_cons_option txt label children)
end

module Markup = Make (Tyxml.Xml) (Tyxml.Svg) (Tyxml.Html)
