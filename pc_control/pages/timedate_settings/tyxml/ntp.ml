open Components_tyxml

let id = "ntp-config"

let ntp_id = "ntp-flag"

let ntp_server_id = "ntp-server"

let ntp_address_id = "ntp-address"

module Make
         (Xml : Xml_sigs.NoWrap)
         (Svg : Svg_sigs.NoWrap with module Xml := Xml)
         (Html : Html_sigs.NoWrap with module Xml := Xml and module Svg := Svg) =
struct
  open Html

  open Form_field.Make (Xml) (Svg) (Html)

  open Switch.Make (Xml) (Svg) (Html)

  open Textfield.Make (Xml) (Svg) (Html)

  open Ui_templates_tyxml.Settings_page.Make (Xml) (Svg) (Html)
  
  let create ?classes ?(a = []) (v : Pc_control_types.Timedate_config.t) =
    let ntp =
      let ntp_input_id = ntp_id ^ "-input" in
      let switch = switch ~input_id:ntp_input_id ~checked:v.ntp () in
      form_field ~a:[ a_id ntp_id ] ~label_for:ntp_input_id ~label:"Автоматически настроить дату и время"
        ~align_end:true ~input:switch ()
    in
    let ntp_server =
      let ntp_server_input_id = ntp_server_id ^ "-input" in
      textfield
        ?value:v.ntp_server
        ~a:[ a_id ntp_server_id ]
        ~readonly:true
        ~input_id:ntp_server_input_id
        ~label:"Сервер NTP"
        ()
    in
    let ntp_address =
      let ntp_address = Option.map Ipaddr.V4.to_string v.ntp_ip in
      let ntp_address_input_id = ntp_address_id ^ "-input" in
      textfield
        ?value:ntp_address
        ~a:[ a_id ntp_address_id ]
        ~readonly:true
        ~input_id:ntp_address_input_id
        ~label:"Адрес сервера NTP"
        ()
    in
    let header =
      create_section_header ~title:(`Text "Автоматическое определение даты и времени") ()
    in
    create_section
      ?classes
      ~a:(a_id id::a)
      ~header
      ~children:[ ntp; ntp_server; ntp_address ]
      ()
  
end

module F = Make (Tyxml.Xml) (Tyxml.Svg) (Tyxml.Html)
