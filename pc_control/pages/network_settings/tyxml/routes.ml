open Components_tyxml

module Make
    (Xml : Xml_sigs.NoWrap)
    (Svg : Svg_sigs.NoWrap with module Xml := Xml)
    (Html : Html_sigs.NoWrap with module Xml := Xml and module Svg := Svg) =
struct
  open Html
  module Button = Button.Make (Xml) (Svg) (Html)
  module Card = Card.Make (Xml) (Svg) (Html)
  module Common_markup = Common.Make (Xml) (Svg) (Html)
  module Item_list = Item_list.Make (Xml) (Svg) (Html)

  let id = "routes-config"

  let create_item ((ip, mask) : Ipaddr.V4.t * int32) =
    let meta = Common_markup.create_remove_button () in
    let text = Printf.sprintf "%s/%ld" (Ipaddr.V4.to_string ip) mask in
    Item_list.create_item ~meta ~primary_text:(`Text text) ()

  let create_list routes = Item_list.create ~children:(List.map create_item routes) ()

  let create ?classes ?(attrs = []) (v : Pc_control_types.Network_config.ipv4_conf) :
      'a elt =
    let add = Button.button ~classes:[Card.CSS.action] ~label:"Добавить" () in
    let empty =
      Unsafe.coerce_elt
      @@ div
           ~a:[a_class [Common.CSS.empty_placeholder]]
           [txt "Статические маршруты не заданы"]
    in
    Common_markup.create_section
      ?classes
      ~attrs:(a_id id :: attrs)
      ~header:
        (Common_markup.create_section_header
           ~title:(`Text "Статические маршруты")
           ())
      ~children:
        [ Card.card_media ~children:[create_list v.routes.static; empty] ()
        ; Card.card_actions ~children:[Card.card_action_buttons ~children:[add] ()] () ]
      ()
end

module Markup = Make (Tyxml.Xml) (Tyxml.Svg) (Tyxml.Html)
