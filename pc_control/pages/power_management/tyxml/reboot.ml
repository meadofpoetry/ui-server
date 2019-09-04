open Components_tyxml

let warning_hint =
  "Hint"

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
      [ Card'.create_media [txt warning_hint] ()
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
