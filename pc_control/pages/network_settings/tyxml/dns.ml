module Make
    (Xml : Xml_sigs.NoWrap)
    (Svg : Svg_sigs.NoWrap with module Xml := Xml)
    (Html : Html_sigs.NoWrap with module Xml := Xml and module Svg := Svg) =
struct
  open Html

  open Components_tyxml.Button.Make (Xml) (Svg) (Html)

  open Components_tyxml.Card.Make (Xml) (Svg) (Html)

  open Components_tyxml.Item_list.Make (Xml) (Svg) (Html)

  module Common_markup = Common.Make (Xml) (Svg) (Html)

  let id = "dns-config"

  let create_item (ip : Ipaddr.V4.t) =
    let meta = Common_markup.create_remove_button () in
    let text = Ipaddr.V4.to_string ip in
    list_item ~meta ~primary_text:(`Text text) ()

  let create_list (ip : Ipaddr.V4.t list) = list ~children:(List.map create_item ip) ()

  let create ?classes ?(a = []) (v : Pc_control_types.Network_config.ipv4_conf) =
    let add =
      button ~classes:[Components_tyxml.Card.CSS.action] ~label:"Добавить" ()
    in
    let empty =
      Unsafe.coerce_elt
      @@ div
           ~a:[a_class [Common.CSS.empty_placeholder]]
           [txt "DNS серверы не заданы"]
    in
    Common_markup.create_section
      ?classes
      ~a:(a_id id :: a)
      ~header:
        (Common_markup.create_section_header ~title:(`Text "DNS серверы") ())
      ~children:
        [ card_media ~children:[create_list v.dns; empty] ()
        ; card_actions ~children:[card_action_buttons ~children:[add] ()] () ]
      ()
end

module F = Make (Tyxml.Xml) (Tyxml.Svg) (Tyxml.Html)
