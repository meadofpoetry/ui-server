open Components_tyxml
open Pipeline_types

module CSS = struct
  let root = "list-of-widgets"
  let item = BEM.add_element root "item"
end

module Make(Xml : Xml_sigs.NoWrap)
    (Svg : Svg_sigs.NoWrap with module Xml := Xml)
    (Html : Html_sigs.NoWrap with module Xml := Xml
                              and module Svg := Svg) = struct
  open Html
  open Utils

  module Widget' = Widget.Make(Xml)(Svg)(Html)
  module Icon_ = Icon.Make(Xml)(Svg)(Html)

  let create_item ?(classes = []) ?attrs ?id (widget : Wm.widget) : 'a elt =
    let classes = CSS.item :: Item_list.CSS.item :: classes in
    let path = Widget.widget_type_to_svg_path widget.type_ in
    let graphic = Icon_.SVG.create [Icon_.SVG.create_path path ()] () in
    li ~a:([ a_class classes
           ; a_draggable true ]
           @ Widget'.to_html_attributes ?id widget
           <@> attrs)
      [ span ~a:[a_class [Item_list.CSS.item_graphic]] [graphic]
      ; span [txt widget.description]
      ]

  let create_list ?(classes = []) ?attrs items : 'a elt =
    let classes = Item_list.CSS.root :: classes in
    ul ~a:([a_class classes] <@> attrs) items

  let create_subheader ?(classes = []) ?attrs title : 'a elt =
    let classes = Item_list.CSS.group_subheader :: classes in
    h3 ~a:([a_class classes] <@> attrs) [txt title]

  let create ?(classes = []) ?attrs content : 'a elt =
    let classes = CSS.root :: Item_list.CSS.group :: classes in
    div ~a:([a_class classes] <@> attrs) content

end
