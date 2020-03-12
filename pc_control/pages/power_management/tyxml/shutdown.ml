open Components_tyxml

module Make
    (Xml : Xml_sigs.NoWrap)
    (Svg : Svg_sigs.NoWrap with module Xml := Xml)
    (Html : Html_sigs.NoWrap with module Xml := Xml and module Svg := Svg) =
struct
  open Html

  open Ui_templates_tyxml.Settings_page.Make (Xml) (Svg) (Html)

  open Button.Make (Xml) (Svg) (Html)

  let create ?classes ?(a = []) () =
    create_section ?classes
      ~a:(Html.a_id "shutdown" :: a)
      ~header:
        (create_section_header
           ~title:(`Text "Выключение питания") ())
      ~children:
        [
          Card_markup.card_media
            ~children:
              [
                div
                  [
                    txt
                      "Нажмите эту кнопку, чтобы \
                       выключить прибор.";
                  ];
                div
                  [
                    span
                      [
                        strong [ txt "Внимание! " ];
                        txt
                          "Выключение приведет к \
                           потере связи с прибором. \
                           Дальнейшая работа с \
                           прибором будет возможна \
                           только после его включения \
                           путём нажатия кнопки на \
                           лицевой панели.";
                      ];
                  ];
              ]
            ();
          hr ~a:[ a_class [ Divider.CSS.root ] ] ();
          Card_markup.card_actions
            ~children:
              [
                Card_markup.card_action_buttons
                  ~children:
                    [
                      button ~appearance:Raised
                        ~label:"Выключить прибор" ();
                    ]
                  ();
              ]
            ();
        ]
      ()
end

module F = Make (Tyxml.Xml) (Tyxml.Svg) (Tyxml.Html)
