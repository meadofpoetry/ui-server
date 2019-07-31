module CSS = struct
  include Ui_templates_tyxml.Settings_page.CSS
  let text_field_container = "text-field-container"

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

    let form_id = "password-config-form"

    let old_password_id = "old-password"

    let new_password_id = "new-password"

    let confirm_password_id = "confirm-password"

    let make_textfield ?(autocomplete = "off") ?value
        ~name ~label ~id () : 'a elt =
      let id' = id ^ "-input" in
      let path = Icon.SVG.create_path Components_tyxml.Svg_icons.eye_off () in
      let icon = Icon.SVG.create
          ~attrs:[ Svg.Unsafe.string_attrib "role" "button"
                 ; Svg.Unsafe.string_attrib "tabindex" "0" ]
          ~classes:[Textfield.CSS.icon]
          [path]
          () in
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
        ~trailing_icon:icon
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
                  ; a_value user
                  ; a_aria "hidden" ["true"]]
        ()

    let make ?(classes = []) ?(attrs = []) () =
      let text_field_container content =
        div ~a:[a_class [CSS.text_field_container]] content in
      let make_helper_text ?(persistent = true) text =
        Textfield.create_helper_line
          [Textfield.Helper_text.create
             ~validation:true
             ~persistent
             ~text
             ()] in
      let submit = Button.create
          ~classes:[Card.CSS.action]
          ~attrs:[a_form form_id]
          ~appearance:Raised
          ~button_type:`Submit
          ~label:"Сменить пароль"
          () in
      let classes = CSS.Password.root :: classes in
      let form_content =
        [ make_username (Application_types.User.to_string `Guest)
        ; text_field_container
            [ make_textfield
                ~id:old_password_id
                ~name:"current_password"
                ~autocomplete:"current-password"
                ~label:"Старый пароль"
                ()
            ; make_helper_text ~persistent:false ""
            ]
        ; text_field_container
            [ make_textfield
                ~id:new_password_id
                ~name:"new_password"
                ~autocomplete:"new-password"
                ~label:"Новый пароль"
                ()
            ; make_helper_text "Минимум 4 символа"
            ]
        ; text_field_container
            [ make_textfield
                ~id:confirm_password_id
                ~name:"confirm_password"
                ~autocomplete:"new-password"
                ~label:"Подтвердите новый пароль"
                ()
            ; make_helper_text ~persistent:false ""
            ]
        ] in
      make_section ~classes ~attrs:(a_id id :: attrs)
        ~header:(make_section_header ~title:"Пароль" [])
        [ make_user_tabs ()
        ; hr ()
        ; Card.create_media [form ~a:[a_id form_id] form_content] ()
        ; Card.create_actions [Card.create_action_buttons [submit] ()] ()
        ]

  end

end
