open Components_tyxml

module CSS = struct
  let root = Util.CSS.root ^ "-service-sdt-info"

  let item = BEM.add_element root "item"
end

module Make
    (Xml : Xml_sigs.NoWrap)
    (Svg : Svg_sigs.NoWrap with module Xml := Xml)
    (Html : Html_sigs.NoWrap with module Xml := Xml and module Svg := Svg) =
struct
  open Html
  module Item_list_markup = Item_list.Make (Xml) (Svg) (Html)

  let create_item_meta ?(classes = []) ?(attrs = []) ~text () =
    span ~a:([a_class classes] @ attrs) [txt text]

  let create_item ?(classes = []) ?attrs ?meta ~primary_text () =
    let classes = CSS.item :: classes in
    let meta =
      match meta with
      | None -> None
      | Some (`Element e) -> Some e
      | Some (`Text s) -> Some (create_item_meta ~text:s ())
    in
    Item_list_markup.create_item ~classes ?attrs ~primary_text ?meta ()

  let create ?(classes = []) ?attrs ?children () =
    let children =
      match children with
      | Some x -> x
      | None ->
          let meta = `Text "" in
          [ create_item
              ~attrs:[a_user_data "type" "name"]
              ~primary_text:(`Text "Имя")
              ~meta
              ()
          ; create_item
              ~attrs:[a_user_data "type" "provider"]
              ~primary_text:(`Text "Провайдер")
              ~meta
              ()
          ; create_item
              ~attrs:[a_user_data "type" "type"]
              ~primary_text:(`Text "Тип")
              ~meta
              ()
          ; create_item
              ~attrs:[a_user_data "type" "eit-schedule"]
              ~primary_text:(`Text "EIT schedule")
              ~meta
              ()
          ; create_item
              ~attrs:[a_user_data "type" "scrambling"]
              ~primary_text:(`Text "Скремблирование")
              ~meta
              ()
          ; create_item
              ~attrs:[a_user_data "type" "eit-pf"]
              ~primary_text:(`Text "EIT P/F")
              ~meta
              ()
          ; create_item
              ~attrs:[a_user_data "type" "running-status"]
              ~primary_text:(`Text "Running status")
              ~meta
              () ]
    in
    Item_list_markup.create
      ~classes
      ?attrs
      ~dense:true
      ~non_interactive:true
      ~children
      ()
end
