open Components_tyxml

module Make
    (Xml : Xml_sigs.NoWrap)
    (Svg : Svg_sigs.NoWrap with module Xml := Xml)
    (Html : Html_sigs.NoWrap with module Xml := Xml and module Svg := Svg) =
struct
  open Html
  module Button = Button.Make (Xml) (Svg) (Html)

  open Ui_templates_tyxml.Settings_page.Make (Xml) (Svg) (Html)

  let make ?classes ?(attrs = []) () =
    make_section
      ?classes
      ~attrs:([Html.a_id "shutdown"] @ attrs)
      ~header:(make_section_header ~title:"Выключение питания" [])
      [ Card'.create_media
          [ div
              [ txt
                  "Нажмите эту кнопку, чтобы выключить \
                   прибор." ]
          ; div
              [ span
                  [ strong [txt "Внимание! "]
                  ; txt
                      "Выключение приведет к потере связи \
                       с прибором. Дальнейшая работа с \
                       прибором будет возможна только \
                       после его включения путём нажатия \
                       кнопки на лицевой панели." ] ] ]
          ()
      ; hr ~a:[a_class [Divider.CSS.root]] ()
      ; Card'.create_actions
          [ Card'.create_action_buttons
              [ Button.create
                  ~appearance:Raised
                  ~label:"Выключить прибор"
                  () ]
              () ]
          () ]
end
