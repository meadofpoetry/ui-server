open Utils

module Make(Xml : Xml_sigs.NoWrap)
         (Svg : Svg_sigs.NoWrap with module Xml := Xml)
         (Html : Html_sigs.NoWrap
          with module Xml := Xml
           and module Svg := Svg) = struct

  module Item_list = Item_list.Make(Xml)(Svg)(Html)

  open Html

  let base_class = "mdc-tree"
  let dense_class = CSS.add_modifier base_class "dense"
  let two_line_class = CSS.add_modifier base_class "two-line"
  let avatar_class = CSS.add_modifier base_class "avatar-list"

  module Item = struct

    let _class = CSS.add_element base_class "item"
    let list_class = CSS.add_element base_class "list"
    let item_open_class = CSS.add_modifier _class "open"
    let list_open_class = CSS.add_modifier list_class "open"

    let create_item ?classes ?attrs ?graphic ?meta content () : 'a elt =
      Item_list.Item.create ?classes ?attrs ?graphic ?meta ~tag:div content ()

    let create_divider ?classes ?attrs ?inset () : 'a elt =
      Item_list.Item.create_divider ?classes ?attrs ?inset ()

    let create ?(classes = []) ?attrs ?nested_list ~item () : 'a elt =
      li ~a:([a_class (_class :: classes)] <@> attrs)
        (item :: (nested_list ^:: []))

  end

  let create ?(classes = []) ?attrs ?(avatar = false) ?(dense = false)
        ?(two_line = false) ~items () : 'a elt =
    let classes =
      classes
      |> cons_if dense dense_class
      |> cons_if two_line two_line_class
      |> cons_if avatar avatar_class
      |> List.cons Item_list.base_class
      |> List.cons base_class in
    ul ~a:([a_class classes] <@> attrs) items

end
