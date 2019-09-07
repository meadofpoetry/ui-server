open Components_tyxml

module Make
    (Xml : Xml_sigs.NoWrap)
    (Svg : Svg_sigs.NoWrap with module Xml := Xml)
    (Html : Html_sigs.NoWrap with module Xml := Xml and module Svg := Svg) =
struct
  open Html

  open Ui_templates_tyxml.Settings_page.Make (Xml) (Svg) (Html)

  module Button_markup = Button.Make (Xml) (Svg) (Html)

  let create ?classes ?(attrs = []) () =
    create_section
      ?classes
      ~attrs:([Html.a_id "reboot"] @ attrs)
      ~header:(create_section_header ~title:"Перезагрузка прибора" [])
      [ Card_markup.create_media
          [ div
              [ txt
                  "Нажмите эту кнопку, чтобы \
                   перезагрузить прибор." ]
          ; div
              [ span
                  [ strong [txt "Внимание! "]
                  ; txt
                      "Перезагрузка приведет к временной \
                       потере связи с прибором." ] ] ]
      ; hr ~a:[a_class [Divider.CSS.root]] ()
      ; Card_markup.create_actions
          [ Card_markup.create_action_buttons
              [ Button_markup.create
                  ~appearance:Raised
                  ~label:"Перезагрузить прибор"
                  () ] ] ]
end

module Markup = Make (Tyxml.Xml) (Tyxml.Svg) (Tyxml.Html)
