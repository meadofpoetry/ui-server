module Make
    (Xml : Xml_sigs.NoWrap)
    (Svg : Svg_sigs.NoWrap with module Xml := Xml)
    (Html : Html_sigs.NoWrap with module Xml := Xml and module Svg := Svg) =
struct
  open Html
  module Button_markup = Components_tyxml.Button.Make (Xml) (Svg) (Html)
  module Card_markup = Components_tyxml.Card.Make (Xml) (Svg) (Html)
  module Common_markup = Common.Make (Xml) (Svg) (Html)
  module Item_list_markup = Components_tyxml.Item_list.Make (Xml) (Svg) (Html)

  let id = "dns-config"

  let create_item (ip : Ipaddr.V4.t) =
    let meta = Common_markup.create_remove_button () in
    let text = Ipaddr.V4.to_string ip in
    Item_list_markup.create_item ~meta (txt text) ()

  let create_list (ip : Ipaddr.V4.t list) =
    let items = List.map create_item ip in
    Item_list_markup.create ~items ()

  let create ?classes ?(attrs = []) (v : Pc_control_types.Network_config.ipv4_conf) :
      'a elt =
    let add =
      Button_markup.create
        ~classes:[Components_tyxml.Card.CSS.action]
        ~label:"Добавить"
        ()
    in
    let empty =
      Unsafe.coerce_elt
      @@ div
           ~a:[a_class [Common.CSS.empty_placeholder]]
           [txt "DNS серверы не заданы"]
    in
    Common_markup.create_section
      ?classes
      ~attrs:(a_id id :: attrs)
      ~header:(Common_markup.create_section_header ~title:"DNS серверы" [])
      [ Card_markup.create_media [create_list v.dns; empty]
      ; Card_markup.create_actions [Card_markup.create_action_buttons [add]] ]
end

module Markup = Make (Tyxml.Xml) (Tyxml.Svg) (Tyxml.Html)
