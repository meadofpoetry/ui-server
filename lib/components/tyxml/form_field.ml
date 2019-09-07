module CSS = struct
  let root = "mdc-form-field"

  let align_end = BEM.add_modifier root "align-end"
end

module Make
    (Xml : Xml_sigs.NoWrap)
    (Svg : Svg_sigs.NoWrap with module Xml := Xml)
    (Html : Html_sigs.NoWrap with module Xml := Xml and module Svg := Svg) =
struct
  open Html

  let create_label ?(classes = []) ?(attrs = []) ?for_id ?label ?(content = []) () =
    Html.label
      ~a:([a_class classes] @ attrs |> Utils.map_cons_option a_label_for for_id)
      (Utils.map_cons_option txt label content)

  let create ?(classes = []) ?(attrs = []) ?(align_end = false) ~input ~label () =
    let (classes : string list) =
      classes |> Utils.cons_if align_end CSS.align_end |> List.cons CSS.root
    in
    div ~a:([a_class classes] @ attrs) [input; label]
end

module Markup = Make (Tyxml.Xml) (Tyxml.Svg) (Tyxml.Html)
