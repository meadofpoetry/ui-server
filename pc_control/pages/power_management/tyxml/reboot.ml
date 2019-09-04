open Components_tyxml

module Make(Xml : Xml_sigs.NoWrap)
    (Svg : Svg_sigs.NoWrap with module Xml := Xml)
    (Html : Html_sigs.NoWrap with module Xml := Xml
                              and module Svg := Svg) = struct
  open Html

  module Button = Button.Make(Xml)(Svg)(Html)

  open Ui_templates_tyxml.Settings_page.Make(Xml)(Svg)(Html)

  let make ?classes ?(attrs = []) () =
    make_section ?classes ~attrs:([Html.a_id "reboot"] @ attrs)
      ~header:(make_section_header ~title:"Перезагрузка прибора" [])
      [ Card'.create_media
          [ div [txt "Нажмите эту кнопку, чтобы перезагрузить прибор."]
          ; div [span
                 [ strong [txt "Внимание! "]
                 ; txt "Перезагрузка приведет к временной потере \
                        связи с прибором."
                 ]
              ]
          ]
          ()
      ; hr ~a:[a_class [Divider.CSS.root]] ()
      ; Card'.create_actions
          [Card'.create_action_buttons
             [ Button.create
                 ~appearance:Raised
                 ~label:"Перезагрузить прибор"
                 ()
             ]
             ()
          ]
          ()
      ]
end
