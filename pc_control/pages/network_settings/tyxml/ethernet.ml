module Make
    (Xml : Xml_sigs.NoWrap)
    (Svg : Svg_sigs.NoWrap with module Xml := Xml)
    (Html : Html_sigs.NoWrap with module Xml := Xml and module Svg := Svg) =
struct
  open Html
  module Common_markup = Common.Make (Xml) (Svg) (Html)

  let id = "ethernet-config"

  let mac_input_id = "mac-address"

  let create ?classes ?(a = []) (v : Pc_control_types.Network_config.ethernet_conf) =
    let value = Macaddr.to_string v.mac_address in
    let mac =
      Common_markup.create_textfield ~value ~label:"MAC адрес" ~id:mac_input_id ()
    in
    Common_markup.create_section
      ?classes
      ~a:(a_id id :: a)
      ~header:
        (Common_markup.create_section_header
           ~title:(`Text "Настройки Ethernet")
           ())
      ~children:[mac]
      ()
end

module F = Make (Tyxml.Xml) (Tyxml.Svg) (Tyxml.Html)
