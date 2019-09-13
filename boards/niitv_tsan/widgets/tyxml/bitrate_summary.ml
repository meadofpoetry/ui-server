open Components_tyxml

module CSS = struct
  let root = Util.CSS.root ^ "-bitrate-summary"

  let value = BEM.add_element root "value"

  let value_total = BEM.add_modifier value "total"

  let value_effective = BEM.add_modifier value "effective"
end

let na = "n/a"

let bitrate_to_string = function
  | None -> na
  | Some x -> Printf.sprintf "%f Мбит/с" x

module Make
    (Xml : Xml_sigs.NoWrap)
    (Svg : Svg_sigs.NoWrap with module Xml := Xml)
    (Html : Html_sigs.NoWrap with module Xml := Xml and module Svg := Svg) =
struct
  open Html
  module Item_list_markup = Item_list.Make (Xml) (Svg) (Html)

  let create_value ?(classes = []) ?attrs ?value text =
    let classes = CSS.value :: classes in
    let meta =
      span ~a:[a_class [Item_list.CSS.item_meta]] [txt (bitrate_to_string value)]
    in
    let primary_text = `Text text in
    Item_list_markup.create_item ~classes ?attrs ~meta ~primary_text ()

  let create ?(classes = []) ?attrs ?total ?effective ?children () =
    let classes = CSS.root :: classes in
    let children =
      match children with
      | Some x -> x
      | None ->
          let total =
            create_value
              ~classes:[CSS.value_total]
              ?value:total
              "Общий битрейт:"
          in
          let effective =
            create_value
              ?value:effective
              ~classes:[CSS.value_effective]
              "Полезный битрейт:"
          in
          [total; effective]
    in
    Item_list_markup.create
      ~classes
      ?attrs
      ~dense:true
      ~non_interactive:true
      ~children
      ()
end

module Markup = Make (Tyxml.Xml) (Tyxml.Svg) (Tyxml.Html)
