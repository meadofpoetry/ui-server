module CSS = struct
  let root = "mdc-tree"
  let item = BEM.add_element root "item"
  let list = BEM.add_element root "list"
  let item_open = BEM.add_modifier item "open"
  let list_open = BEM.add_modifier list "open"

  let dense = BEM.add_modifier root "dense"
  let two_line = BEM.add_modifier root "two-line"
  let avatar = BEM.add_modifier root "avatar-list"
end

module Make(Xml : Xml_sigs.NoWrap)
         (Svg : Svg_sigs.NoWrap with module Xml := Xml)
         (Html : Html_sigs.NoWrap
          with module Xml := Xml
           and module Svg := Svg) = struct
  open Html
  open Utils

  module Lst = Item_list.Make(Xml)(Svg)(Html)

  let create_list_item ?classes ?attrs ?graphic ?meta content () : 'a elt =
    Lst.create_item ?classes ?attrs ?graphic ?meta content ()

  let create_divider ?classes ?attrs ?inset ~tag () : 'a elt =
    Lst.create_divider ?classes ?attrs ?inset ~tag ()

  let create_item ?(classes = []) ?attrs ?nested_list ~item () : 'a elt =
    li ~a:([a_class (CSS.item :: classes)] <@> attrs)
      (item :: (nested_list ^:: []))

  let create ?(classes = []) ?attrs ?(avatar = false) ?(dense = false)
        ?(two_line = false) ~items () : 'a elt =
    let classes =
      classes
      |> cons_if dense CSS.dense
      |> cons_if two_line CSS.two_line
      |> cons_if avatar CSS.avatar
      |> List.cons Item_list.CSS.root
      |> List.cons CSS.root in
    ul ~a:([a_class classes] <@> attrs) items

end
