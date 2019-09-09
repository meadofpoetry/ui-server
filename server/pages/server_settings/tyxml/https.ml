open Components_tyxml

let id = "https-config"

let enable_id = "https-enable"

module Make
    (Xml : Xml_sigs.NoWrap)
    (Svg : Svg_sigs.NoWrap with module Xml := Xml)
    (Html : Html_sigs.NoWrap with module Xml := Xml and module Svg := Svg) =
struct
  open Html
  module Button_markup = Button.Make (Xml) (Svg) (Html)
  module Card_markup = Card.Make (Xml) (Svg) (Html)
  module Form_field_markup = Form_field.Make (Xml) (Svg) (Html)
  module Switch_markup = Switch.Make (Xml) (Svg) (Html)

  open Ui_templates_tyxml.Settings_page.Make (Xml) (Svg) (Html)

  let create ?classes ?(attrs = []) (v : Server_types.settings) : 'a elt =
    let submit =
      Button_markup.create ~classes:[Card.CSS.action] ~label:"Применить" ()
    in
    let enable_input_id = enable_id ^ "-input" in
    let switch =
      Switch_markup.create ~input_id:enable_input_id ~checked:v.https_enabled ()
    in
    let enable =
      Form_field_markup.create
        ~attrs:[a_id enable_id]
        ~label:(`Text "Использовать HTTPS протокол")
        ~label_for:enable_input_id
        ~align_end:true
        ~input:switch
        ()
    in
    create_section
      ?classes
      ~attrs:(a_id id :: attrs)
      ~header:(create_section_header ~title:(`Text "HTTPS") ())
      ~children:
        [ Card_markup.create_media ~children:[enable] ()
        ; Card_markup.create_actions
            ~children:[Card_markup.create_action_buttons ~children:[submit] ()]
            () ]
      ()
end

module Markup = Make (Tyxml.Xml) (Tyxml.Svg) (Tyxml.Html)
