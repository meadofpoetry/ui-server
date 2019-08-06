open Components_tyxml
open Pipeline_types

module CSS = struct
  let root = "list-of-widgets"
  let item = BEM.add_element root "item"
  (* FIXME define 'mdc-placeholder' class in 'ui-templates-tyxml' lib *)
  let placeholder_hidden = BEM.add_modifier "mdc-placeholder" "hidden"
end

module Make(Xml : Xml_sigs.NoWrap)
    (Svg : Svg_sigs.NoWrap with module Xml := Xml)
    (Html : Html_sigs.NoWrap with module Xml := Xml
                              and module Svg := Svg) = struct
  open Html

  module Widget' = Widget.Make(Xml)(Svg)(Html)
  module Icon_ = Icon.Make(Xml)(Svg)(Html)

  let create_item ?(classes = []) ?(attrs = []) ?id (widget : Wm.widget) : 'a elt =
    let classes = CSS.item :: Item_list.CSS.item :: classes in
    let path = Widget.widget_type_to_svg_path widget.type_ in
    let graphic = Icon_.SVG.create [Icon_.SVG.create_path path ()] () in
    let typ = match widget.type_, widget.aspect with
      | Wm.Video, None -> "Видео"
      | Video, Some (w, h) -> Printf.sprintf "Видео %dx%d" w h
      | Audio, _ -> "Аудио" in
    let secondary = match widget.pid with
      | None -> typ
      | Some x -> Printf.sprintf "%s, PID %d" typ x in
    li ~a:([ a_class classes
           ; a_draggable true ]
           @ Widget'.to_html_attributes ?id widget
           @ attrs)
      [ span ~a:[a_class [Item_list.CSS.item_graphic]] [graphic]
      ; span ~a:[a_class [Item_list.CSS.item_text]]
          [ span ~a:[a_class [Item_list.CSS.item_primary_text]]
              [txt widget.description]
          ; span ~a:[a_class [Item_list.CSS.item_secondary_text]]
              [txt secondary]
          ]
      ]

  let create_list ?(classes = []) ?(attrs = []) domain items : 'a elt =
    let classes = Item_list.CSS.root :: Item_list.CSS.two_line :: classes in
    ul ~a:([ a_class classes
           ; a_user_data "domain" (Widget.domain_attr_value domain)
           ] @ attrs) items

  let create_subheader ?(classes = []) ?(attrs = []) domain title : 'a elt =
    let classes = Item_list.CSS.group_subheader :: classes in
    h3 ~a:([ a_class classes
           ; a_user_data "domain" (Widget.domain_attr_value domain)
           ] @ attrs) [txt title]

  let create ?(classes = []) ?(attrs = []) content : 'a elt =
    let classes = CSS.root :: Item_list.CSS.group :: classes in
    div ~a:([a_class classes] @ attrs) content

end
