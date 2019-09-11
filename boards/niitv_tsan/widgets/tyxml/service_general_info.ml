open Components_tyxml

module CSS = struct
  let root = Util.CSS.root ^ "-service-general-info"

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
              ~attrs:[a_user_data "type" "service-id"]
              ~primary_text:(`Text "Service ID")
              ~meta
              ()
          ; create_item
              ~attrs:[a_user_data "type" "pmt-pid"]
              ~primary_text:(`Text "PMT PID")
              ~meta
              ()
          ; create_item
              ~attrs:[a_user_data "type" "pcr-pid"]
              ~primary_text:(`Text "PCR PID")
              ~meta
              ()
          ; create_item
              ~attrs:[a_user_data "type" "bitratenow"]
              ~primary_text:(`Text "Битрейт")
              ~meta
              ()
          ; create_item
              ~attrs:[a_user_data "type" "bitratemin"]
              ~primary_text:(`Text "Min")
              ~meta
              ()
          ; create_item
              ~attrs:[a_user_data "type" "bitratemax"]
              ~primary_text:(`Text "Max")
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
