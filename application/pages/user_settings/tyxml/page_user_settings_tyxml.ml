module CSS = struct
  include Ui_templates_tyxml.Settings_page.CSS
  module Password = struct
    let root = "password-section"
  end
end

module Make(Xml : Xml_sigs.NoWrap)
    (Svg : Svg_sigs.NoWrap with module Xml := Xml)
    (Html : Html_sigs.NoWrap with module Xml := Xml
                              and module Svg := Svg) = struct
  module Components = Components_tyxml.Bundle.Make(Xml)(Svg)(Html)
  open Html
  open Components

  include Ui_templates_tyxml.Settings_page.Make(Xml)(Svg)(Html)

  module Password = struct

    let id = "password-config"

    let make_textfield ?value ~label ~id () : 'a elt =
      let id' = id ^ "-input" in
      let input = Unsafe.coerce_elt @@ Textfield.create_input ~id:id' ?value () in
      let label = Floating_label.create
          ~classes:(match value with
              | None -> []
              | Some _ -> [Components_tyxml.Floating_label.CSS.float_above])
          ~for_:id' label () in
      let line_ripple = Line_ripple.create () in
      Textfield.create
        ~attrs:[a_id id]
        ~input
        ~label
        ~line_ripple
        ()

    let make ?classes ?(attrs = []) () =
      let make_helper_text ?(persistent = true) text =
        Textfield.Helper_text.create
          ~validation:true
          ~persistent
          ~text
          () in
      let submit = Button.create
          ~classes:[Card.CSS.action]
          ~appearance:Raised
          ~label:"Сменить пароль"
          () in
      make_section ?classes ~attrs:(a_id id :: attrs)
        ~header:(make_section_header ~title:"Пароль" [])
        [ Card.create_media ~classes:[CSS.Password.root]
            [ make_textfield ~label:"Старый пароль" ~id:"old-password" ()
            ; make_helper_text ~persistent:false ""
            ; make_textfield ~label:"Новый пароль" ~id:"new-password" ()
            ; make_helper_text "Минимум 4 символа"
            ; make_textfield
                ~label:"Подтвердите новый пароль"
                ~id:"new-password-confirm"
                ()
            ; make_helper_text ~persistent:false ""
            ]
            ()
        ; Card.create_actions [Card.create_action_buttons [submit] ()] ()
        ]

  end

end
