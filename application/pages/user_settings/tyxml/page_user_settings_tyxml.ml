open Components_tyxml
open Application_types

module CSS = struct
  include Ui_templates_tyxml.Settings_page.CSS
  let text_field_container = "text-field-container"

  module Account = struct
    let greetings = "greetings"
    let permissions = "permissions"
    let accounts_info_link = "accounts-info-link"
    let account_info = "account-info"
    let account_info_title = BEM.add_element account_info "title"
    let account_info_text = BEM.add_element account_info "text"
  end

  module Password = struct
    let root = "password-section"
    let slider = "slider"
  end
end

module Make(Xml : Xml_sigs.NoWrap)
    (Svg : Svg_sigs.NoWrap with module Xml := Xml)
    (Html : Html_sigs.NoWrap with module Xml := Xml
                              and module Svg := Svg) = struct
  module Components = Bundle.Make(Xml)(Svg)(Html)
  open Html
  open Components

  include Ui_templates_tyxml.Settings_page.Make(Xml)(Svg)(Html)

  let pp_user_human ppf = function
    | `Guest -> Format.pp_print_string ppf "Гость"
    | `Operator -> Format.pp_print_string ppf "Оператор"
    | `Root -> Format.pp_print_string ppf "Администратор"

  module Account = struct

    let id = "account"

    let permissions ?(pesonal_appeal = true) = function
      | `Guest ->
        let title = if pesonal_appeal then "Вам" else "Гостю" in
        Printf.sprintf
          "%s предоставлена возможность просмотра результатов измерений. \
           Данная учётная запись не позволяет управлять параметрами мониторинга \
           и настройками прибора."
          title
      | `Operator ->
        let title = if pesonal_appeal then "Вам" else "Оператору" in
        Printf.sprintf
          "%s предоставлена возможность просмотра результатов измерений и \
           конфигурации параметров мониторинга. \
           Данная учётная запись не позволяет управлять настройками прибора."
          title
      | `Root ->
        let title = if pesonal_appeal then "Вам" else "Администратору" in
        Printf.sprintf
          "%s предоставлена возможность полного контроля над всеми функциями и \
           настройками прибора."
          title

    let make_greetings ?(classes = []) ?(attrs = []) user =
      let classes = CSS.Account.greetings :: classes in
      let username_human = Format.asprintf "%a" pp_user_human user in
      let text = Printf.sprintf "Добро пожаловать, %s!" username_human in
      div ~a:([a_class classes] @ attrs) [txt text]

    let make_permissions ?(classes = []) ?(attrs = []) user =
      let classes = CSS.Account.permissions :: classes in
      let text = permissions user in
      div ~a:([a_class classes] @ attrs) [txt text]

    let make_accounts_info_link ?(classes = []) ?(attrs = []) () =
      let classes = CSS.Account.accounts_info_link :: classes in
      span ~a:([a_class classes] @ attrs)
        [txt "Узнать больше об учётных записях"]

    let make ?(classes = []) ?(attrs = []) user =
      let change = Button.create
          ~classes:[Card.CSS.action]
          ~appearance:Raised
          ~button_type:`Submit
          ~label:"Сменить аккаунт"
          () in
      let path = Icon.SVG.create_path Svg_icons.logout_variant () in
      let icon = Icon.SVG.create ~classes:[Button.CSS.icon] [path] () in
      let _exit = Button.create ~icon ~label:"Выйти" () in
      make_section ~classes ~attrs:(a_id id :: attrs)
        ~header:(make_section_header ~title:"Аккаунт" [])
        [ Card.create_media
            [ make_greetings user
            ; make_permissions user
            ; make_accounts_info_link ()
            ] ()
        ; Card.create_actions
            [ Card.create_action_buttons [change] ()
            ; Card.create_action_icons [_exit] ()]
            ()
        ]

  end

  module Password = struct

    let id = "password-config"

    let form_id user = "password-config-form-" ^ (User.to_string user)

    let old_pass_id user = "old-password-" ^ (User.to_string user)

    let new_pass_id user = "new-password-" ^ (User.to_string user)

    let confirm_pass_id user = "confirm-password-" ^ (User.to_string user)

    let make_textfield ?(autocomplete = "off") ?value
        ~name ~label ~id () : 'a elt =
      let id' = id ^ "-input" in
      let path = Icon.SVG.create_path Svg_icons.eye_off () in
      let icon =
        div ~a:[ a_class [Textfield.CSS.icon]
               ; a_role ["button"]
               ; a_tabindex 0 ]
          [Icon.SVG.create [path] ()] in
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
              | Some _ -> [Floating_label.CSS.float_above])
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
        let indicator = Tab_indicator.create ?active
            (Tab_indicator.create_content ())
            () in
        let username_human = Format.asprintf "%a" pp_user_human user in
        let text_label = Tab.create_text_label username_human () in
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

    let make_helper_text ?(persistent = true) text =
      Textfield.create_helper_line
        [Textfield.Helper_text.create
           ~validation:true
           ~persistent
           ~text
           ()]

    let make_form user =
      let text_field_container content =
        div ~a:[a_class [CSS.text_field_container]] content in
      let username = Application_types.User.to_string user in
      form ~a:[a_user_data "username" username]
        [ make_username username
        ; text_field_container
            [ make_textfield
                ~id:(old_pass_id user)
                ~name:"current_password"
                ~autocomplete:"current-password"
                ~label:"Старый пароль"
                ()
            ; make_helper_text ~persistent:false ""
            ]
        ; text_field_container
            [ make_textfield
                ~id:(new_pass_id user)
                ~name:"new_password"
                ~autocomplete:"new-password"
                ~label:"Новый пароль"
                ()
            ; make_helper_text "Минимум 4 символа"
            ]
        ; text_field_container
            [ make_textfield
                ~id:(confirm_pass_id user)
                ~name:"confirm_password"
                ~autocomplete:"new-password"
                ~label:"Подтвердите новый пароль"
                ()
            ; make_helper_text ~persistent:false ""
            ]
        ; input ~a:([a_input_type `Submit]) ()
        ]

    let make ?(classes = []) ?(attrs = []) () =
      let submit = Button.create
          ~classes:[Card.CSS.action]
          ~appearance:Raised
          ~button_type:`Submit
          ~label:"Сменить пароль"
          () in
      let classes = CSS.Password.root :: classes in
      make_section ~classes ~attrs:(a_id id :: attrs)
        ~header:(make_section_header ~title:"Пароли" [])
        [ make_user_tabs ()
        ; hr ()
        ; Card.create_media
            [div ~a:[a_class [CSS.Password.slider]]
               [ make_form `Guest
               ; make_form `Operator
               ; make_form `Root ]]
            ()
        ; Card.create_actions [Card.create_action_buttons [submit] ()] ()
        ]

  end

end
