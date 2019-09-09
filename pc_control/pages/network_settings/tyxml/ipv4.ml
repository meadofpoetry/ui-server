open Components_tyxml

let id = "ip-config"

let dhcp_id = "dhcp"

let ip_address_input_id = "ip-address"

let mask_input_id = "subnet-mask"

let gateway_input_id = "gateway"

module Make
    (Xml : Xml_sigs.NoWrap)
    (Svg : Svg_sigs.NoWrap with module Xml := Xml)
    (Html : Html_sigs.NoWrap with module Xml := Xml and module Svg := Svg) =
struct
  open Html
  module Common_markup = Common.Make (Xml) (Svg) (Html)
  module Form_field_markup = Form_field.Make (Xml) (Svg) (Html)
  module Switch_markup = Switch.Make (Xml) (Svg) (Html)

  let create ?classes ?(attrs = []) (v : Pc_control_types.Network_config.ipv4_conf) :
      'a elt =
    let ip_value = Ipaddr.V4.to_string (fst v.address) in
    let mask_value =
      Ipaddr.V4.to_string @@ Ipaddr.V4.Prefix.mask @@ Int32.to_int @@ snd v.address
    in
    let gateway_value =
      match v.routes.gateway with
      | None -> None
      | Some x -> Some (Ipaddr.V4.to_string x)
    in
    let dhcp =
      let checked =
        match v.meth with
        | Manual -> false
        | Auto -> true
      in
      let dhcp_input_id = dhcp_id ^ "-input" in
      let switch = Switch_markup.create ~input_id:dhcp_input_id ~checked () in
      Form_field_markup.create
        ~attrs:[a_id dhcp_id]
        ~label_for:dhcp_input_id
        ~label:(`Text "DHCP")
        ~align_end:true
        ~input:switch
        ()
    in
    let ip_address =
      Common_markup.create_textfield
        ~value:ip_value
        ~label:"IP адрес"
        ~id:ip_address_input_id
        ()
    in
    let subnet_mask =
      Common_markup.create_textfield
        ~value:mask_value
        ~label:"Маска подсети"
        ~id:mask_input_id
        ()
    in
    let gateway =
      Common_markup.create_textfield
        ?value:gateway_value
        ~label:"Шлюз"
        ~id:gateway_input_id
        ()
    in
    Common_markup.create_section
      ?classes
      ~attrs:(a_id id :: attrs)
      ~header:
        (Common_markup.create_section_header ~title:(`Text "Настройки IP") ())
      ~children:[dhcp; ip_address; subnet_mask; gateway]
      ()
end

module Markup = Make (Tyxml.Xml) (Tyxml.Svg) (Tyxml.Html)
