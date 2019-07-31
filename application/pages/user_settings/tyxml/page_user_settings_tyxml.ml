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

    let old_password_id = "old-password"

    let new_password_id = "new-password"

    let confirm_password_id = "confirm-password"

    let make_textfield ?(autocomplete = "off") ?value
        ~name ~label ~id () : 'a elt =
      let id' = id ^ "-input" in
      let input =
        Unsafe.coerce_elt
        @@ Textfield.create_input
          ~id:id'
          ~attrs:[ Unsafe.string_attrib "autocomplete" autocomplete
                 ; Unsafe.string_attrib "spellcheck" "false"
                 ; a_name name ]
          ~typ:`Password
          ?value () in
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

    let make_user_tabs () =
      let create_tab ?active user =
        let username = Application_types.User.to_string user in
        let human = match user with
          | `Guest -> "Гость"
          | `Operator -> "Оператор"
          | `Root -> "Администратор" in
        let indicator = Tab_indicator.create ?active
            (Tab_indicator.create_content ())
            () in
        let text_label = Tab.create_text_label human () in
        Tab.create ?active
          ~attrs:[a_user_data "username" username]
          ~indicator
          (Tab.create_content ~text_label ())
          () in
      let tabs =
        [ create_tab ~active:true `Guest
        ; create_tab `Operator
        ; create_tab `Root
        ] in
      let scroll_area =
        Tab_scroller.create_scroll_area
          ~content:(Tab_scroller.create_scroll_content tabs ())
          () in
      let scroller = Tab_scroller.create ~scroll_area () in
      Tab_bar.create ~scroller ()

    let make_username user =
      Unsafe.coerce_elt
      @@ input ~a:[ Unsafe.string_attrib "autocomplete" "username"
                  ; a_input_type `Text
                  ; a_name "username"
                  ; a_value user ]
        ()

    let make ?(classes = []) ?(attrs = []) () =
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
      let classes = CSS.Password.root :: classes in
      make_section ~classes ~attrs:(a_id id :: attrs)
        ~header:(make_section_header ~title:"Пароль" [])
        [ make_user_tabs ()
        ; Card.create_media ~tag:form
            [ make_username (Application_types.User.to_string `Guest)
            ; make_textfield
                ~id:old_password_id
                ~name:"current_password"
                ~autocomplete:"current-password"
                ~label:"Старый пароль"
                ()
            ; make_helper_text ~persistent:false ""
            ; make_textfield
                ~id:new_password_id
                ~name:"new_password"
                ~autocomplete:"new-password"
                ~label:"Новый пароль"
                ()
            ; make_helper_text "Минимум 4 символа"
            ; make_textfield
                ~id:confirm_password_id
                ~name:"confirm_password"
                ~autocomplete:"new-password"
                ~label:"Подтвердите новый пароль"
                ()
            ; make_helper_text ~persistent:false ""
            ]
            ()
        ; Card.create_actions [Card.create_action_buttons [submit] ()] ()
        ]

  end

end
