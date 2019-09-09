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
    Item_list_markup.create_item ~meta ~primary_text:(`Text text) ()

  let create_list (ip : Ipaddr.V4.t list) =
    Item_list_markup.create ~children:(List.map create_item ip) ()

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
      ~header:
        (Common_markup.create_section_header ~title:(`Text "DNS серверы") ())
      ~children:
        [ Card_markup.create_media ~children:[create_list v.dns; empty] ()
        ; Card_markup.create_actions
            ~children:[Card_markup.create_action_buttons ~children:[add] ()]
            () ]
      ()
end

module Markup = Make (Tyxml.Xml) (Tyxml.Svg) (Tyxml.Html)
