open Utils

module Make(Xml : Xml_sigs.NoWrap)
         (Svg : Svg_sigs.NoWrap with module Xml := Xml)
         (Html : Html_sigs.NoWrap
          with module Xml := Xml
           and module Svg := Svg) = struct

  module Item_list = Item_list.Make(Xml)(Svg)(Html)

  open Html

  let base_class  = "mdc-tree"
  let dense_class = Item_list.dense_class

  module Item = struct

    let _class            = CSS.add_element base_class "item"
    let list_class        = CSS.add_element base_class "list"
    let item_open_class   = CSS.add_modifier _class "open"
    let list_open_class   = CSS.add_modifier list_class "open"

    let create_item ?classes ?attrs ?graphic ?meta content () =
      Item_list.Item.create ?classes ?attrs ?graphic ?meta content ()
    
    let create_divider ?classes ?attrs ?inset () =
      Item_list.Item.create_divider ?classes ?attrs ?inset ()

    let create ?(classes=[]) ?attrs ?nested_list ~item () =
      Html.div ~a:([ a_class (_class :: classes) ] <@> attrs)
        (cons_option nested_list []
         |> List.cons item)

  end

  let create ?(classes=[]) ?attrs ?avatar ?dense ?two_line ~items () =
    Item_list.create ~classes:(base_class::classes) ?attrs ?avatar ?dense ?two_line ~items ()

end
