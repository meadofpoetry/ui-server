open Components_tyxml
open Pc_control_types

module CSS = struct
  let root = "settings-section"
  let title = BEM.add_element root "title"
  let header = BEM.add_element root "header"
  let dialog = BEM.add_element root "dialog"
  let empty_placeholder = BEM.add_element root "empty-placeholder"
end

module Make(Xml : Xml_sigs.NoWrap)
    (Svg : Svg_sigs.NoWrap with module Xml := Xml)
    (Html : Html_sigs.NoWrap with module Xml := Xml
                              and module Svg := Svg) = struct
  module Components = Components_tyxml.Bundle.Make(Xml)(Svg)(Html)

  open Html
  open Utils
  open Components

  let make_remove_button () =
    let path = Icon.SVG.create_path Svg_icons.delete () in
    Icon_button.create
      ~classes:[Item_list.CSS.item_meta]
      ~icon:Icon.SVG.(create [path] ())
      ()

  let make_textfield ?value ~label ~id () : 'a elt =
    let id' = id ^ "-input" in
    let input = Unsafe.coerce_elt @@ Textfield.create_input ~id:id' ?value () in
    let label = Floating_label.create
        ~classes:(match value with
            | None -> []
            | Some _ -> [Components_tyxml.Floating_label.CSS.float_above])
        ~for_:id' label () in
    let line_ripple = Line_ripple.create () in
    Textfield.create
      ~attrs:[a_id id]
      ~input
      ~label
      ~line_ripple
      ()

  let make_title ?(classes = []) ?attrs title : 'a elt =
    let classes = CSS.title :: classes in
    h2 ~a:([a_class classes] <@> attrs) [txt title]

  let make_header ?(classes = []) ?attrs ?title content : 'a elt =
    let classes = CSS.header :: classes in
    div ~a:([a_class classes] <@> attrs)
      (match title with
       | None -> content
       | Some x -> make_title x :: content)

  let make ?(classes = []) ?attrs ~header content : 'a elt =
    let classes = CSS.root :: classes in
    div ~a:([a_class classes] <@> attrs)
      (header :: Card.create content :: [])

  module Ethernet = struct

    let id = "ethernet-config"

    let mac_input_id = "mac-address"

    let make ?classes ?(attrs = []) (v : Network_config.ethernet_conf) : 'a elt =
      let value = Macaddr.to_string v.mac_address in
      let mac = make_textfield ~value
          ~label:"MAC адрес"
          ~id:mac_input_id
          () in
      make ?classes ~attrs:(a_id id :: attrs)
        ~header:(make_header ~title:"Настройки Ethernet" [])
        [mac]

  end

  module IPV4 = struct

    let id = "ip-config"

    let dhcp_id = "dhcp"

    let ip_address_input_id = "ip-address"

    let mask_input_id = "subnet-mask"

    let gateway_input_id = "gateway"

    let make ?classes ?(attrs = []) (v : Network_config.ipv4_conf) : 'a elt =
      let ip_value = Ipaddr.V4.to_string (fst v.address) in
      let mask_value =
        Ipaddr.V4.to_string
        @@ Ipaddr.V4.Prefix.mask
        @@ Int32.to_int
        @@ snd v.address in
      let gateway_value = match v.routes.gateway with
        | None -> None
        | Some x -> Some (Ipaddr.V4.to_string x) in
      let dhcp =
        let id' = dhcp_id ^ "-input" in
        let checked = match v.meth with
          | Manual -> false
          | Auto -> true in
        let label = Form_field.create_label ~for_id:id' "DHCP" () in
        let switch = Switch.create
            ~input_id:id'
            ~checked
            () in
        Form_field.create ~attrs:[a_id dhcp_id]
          ~label
          ~align_end:true
          ~input:switch
          () in
      let ip_address = make_textfield ~value:ip_value
          ~label:"IP адрес"
          ~id:ip_address_input_id
          () in
      let subnet_mask = make_textfield ~value:mask_value
          ~label:"Маска подсети"
          ~id:mask_input_id
          () in
      let gateway = make_textfield ?value:gateway_value
          ~label:"Шлюз"
          ~id:gateway_input_id
          () in
      make ?classes ~attrs:(a_id id :: attrs)
        ~header:(make_header ~title:"Настройки IP" [])
        [ dhcp
        ; ip_address
        ; subnet_mask
        ; gateway
        ]

  end

  module DNS = struct
    let id = "dns-config"

    let make_item (ip : Ipaddr.V4.t) =
      let meta = make_remove_button () in
      let text = Ipaddr.V4.to_string ip in
      Item_list.create_item ~meta (txt text) ()

    let make_list (ip : Ipaddr.V4.t list) =
      let items = List.map make_item ip in
      Item_list.create ~items ()

    let make ?classes ?(attrs = []) (v : Network_config.ipv4_conf) : 'a elt =
      let add = Button.create
          ~classes:[Card.CSS.action]
          ~label:"Добавить"
          () in
      let empty =
        Unsafe.coerce_elt
        @@ div ~a:[a_class [CSS.empty_placeholder]]
          [txt "DNS серверы не заданы"] in
      make ?classes ~attrs:(a_id id :: attrs)
        ~header:(make_header ~title:"DNS серверы" [])
        [ Card.create_media [make_list v.dns; empty] ()
        ; Card.create_actions [Card.create_action_buttons [add] ()] ()
        ]
  end

  module Routes = struct

    let id = "routes-config"

    let make_item (ip, mask : Ipaddr.V4.t * int32) =
      let meta = make_remove_button () in
      let text = Printf.sprintf "%s/%ld" (Ipaddr.V4.to_string ip) mask in
      Item_list.create_item ~meta (txt text) ()

    let make_list routes =
      let items = List.map make_item routes in
      Item_list.create ~items ()

    let make ?classes ?(attrs = []) (v : Network_config.ipv4_conf) : 'a elt =
      let add = Button.create
          ~classes:[Card.CSS.action]
          ~label:"Добавить"
          () in
      let empty =
        Unsafe.coerce_elt
        @@ div ~a:[a_class [CSS.empty_placeholder]]
          [txt "Статические маршруты не заданы"] in
      make ?classes ~attrs:(a_id id :: attrs)
        ~header:(make_header ~title:"Статические маршруты" [])
        [ Card.create_media [make_list v.routes.static; empty] ()
        ; Card.create_actions [Card.create_action_buttons [add] ()] ()
        ]

  end

end
