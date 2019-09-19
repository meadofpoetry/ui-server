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
      ~attrs:([Html.a_id "shutdown"] @ attrs)
      ~header:
        (create_section_header ~title:(`Text "Выключение питания") ())
      ~children:
        [ Card.card_media
            ~children:
              [ div
                  [ txt
                      "Нажмите эту кнопку, чтобы \
                       выключить прибор." ]
              ; div
                  [ span
                      [ strong [txt "Внимание! "]
                      ; txt
                          "Выключение приведет к потере \
                           связи с прибором. Дальнейшая \
                           работа с прибором будет возможна \
                           только после его включения путём \
                           нажатия кнопки на лицевой панели."
                      ] ] ]
            ()
        ; hr ~a:[a_class [Divider.CSS.root]] ()
        ; Card.card_actions
            ~children:
              [ Card.card_action_buttons
                  ~children:
                    [ Button.button
                        ~appearance:Raised
                        ~label:"Выключить прибор"
                        () ]
                  () ]
            () ]
      ()
end

module Markup = Make (Tyxml.Xml) (Tyxml.Svg) (Tyxml.Html)
