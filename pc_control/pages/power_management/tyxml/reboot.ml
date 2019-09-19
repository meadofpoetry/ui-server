open Components_tyxml

module Make
    (Xml : Xml_sigs.NoWrap)
    (Svg : Svg_sigs.NoWrap with module Xml := Xml)
    (Html : Html_sigs.NoWrap with module Xml := Xml and module Svg := Svg) =
struct
  open Html

  open Ui_templates_tyxml.Settings_page.Make (Xml) (Svg) (Html)

  module Button = Button.Make (Xml) (Svg) (Html)

  let create ?classes ?(attrs = []) () =
    create_section
      ?classes
      ~attrs:([Html.a_id "reboot"] @ attrs)
      ~header:
        (create_section_header
           ~title:(`Text "Перезагрузка прибора")
           ())
      ~children:
        [ Card.card_media
            ~children:
              [ div
                  [ txt
                      "Нажмите эту кнопку, чтобы \
                       перезагрузить прибор." ]
              ; div
                  [ span
                      [ strong [txt "Внимание! "]
                      ; txt
                          "Перезагрузка приведет к \
                           временной потере связи с \
                           прибором." ] ] ]
            ()
        ; hr ~a:[a_class [Divider.CSS.root]] ()
        ; Card.card_actions
            ~children:
              [ Card.card_action_buttons
                  ~children:
                    [ Button.button
                        ~appearance:Raised
                        ~label:"Перезагрузить прибор"
                        () ]
                  () ]
            () ]
      ()
end

module F = Make (Tyxml.Xml) (Tyxml.Svg) (Tyxml.Html)
