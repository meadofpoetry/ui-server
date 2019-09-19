open Components_tyxml

module CSS = struct
  let greetings = "greetings"

  let permissions = "permissions"

  let accounts_info_link = "accounts-info-link"

  let account_info = "account-info"

  let account_info_title = BEM.add_element account_info "title"

  let account_info_text = BEM.add_element account_info "text"
end

let permissions ?(pesonal_appeal = true) = function
  | `Guest ->
      let title = if pesonal_appeal then "Вам" else "Гостю" in
      Printf.sprintf
        "%s предоставлена возможность просмотра \
         результатов измерений. Данная учётная \
         запись не позволяет управлять параметрами \
         мониторинга и настройками прибора."
        title
  | `Operator ->
      let title = if pesonal_appeal then "Вам" else "Оператору" in
      Printf.sprintf
        "%s предоставлена возможность просмотра \
         результатов измерений и конфигурации \
         параметров мониторинга. Данная учётная \
         запись не позволяет управлять настройками \
         прибора."
        title
  | `Root ->
      let title = if pesonal_appeal then "Вам" else "Администратору" in
      Printf.sprintf
        "%s предоставлена возможность полного \
         контроля над всеми функциями и \
         настройками прибора."
        title

module Make
    (Xml : Xml_sigs.NoWrap)
    (Svg : Svg_sigs.NoWrap with module Xml := Xml)
    (Html : Html_sigs.NoWrap with module Xml := Xml and module Svg := Svg) =
struct
  open Html

  open Ui_templates_tyxml.Settings_page.Make (Xml) (Svg) (Html)

  module Button = Button.Make (Xml) (Svg) (Html)
  module Icon = Icon.Make (Xml) (Svg) (Html)

  let id = "account"

  let create_greetings ?(classes = []) ?(attrs = []) user =
    let classes = CSS.greetings :: classes in
    let username_human = Format.asprintf "%a" Util.pp_user_human user in
    let icon = Icon.SVG.icon ~d:(Util.user_icon_path user) () in
    let text = Printf.sprintf "Добро пожаловать, %s!" username_human in
    div ~a:([a_class classes] @ attrs) [icon; txt text]

  let create_permissions ?(classes = []) ?(attrs = []) user =
    let classes = CSS.permissions :: classes in
    let text = permissions user in
    div ~a:([a_class classes] @ attrs) [txt text]

  let create_accounts_info_link ?(classes = []) ?(attrs = []) () =
    let classes = CSS.accounts_info_link :: classes in
    span
      ~a:([a_class classes] @ attrs)
      [txt "Узнать больше об учётных записях"]

  let create ?(classes = []) ?(attrs = []) user =
    let _exit =
      Button.button_a
        ~classes:[Card.CSS.action]
        ~appearance:Raised
        ~href:(uri_of_string "/logout")
        ~label:"Выйти"
        ()
    in
    create_section
      ~classes
      ~attrs:(a_id id :: attrs)
      ~header:(create_section_header ~title:(`Text "Аккаунт") ())
      ~children:
        [ Card.card_media
            ~children:
              [ create_greetings user
              ; create_permissions user
              ; create_accounts_info_link () ]
            ()
        ; Card.card_actions ~children:[Card.card_action_buttons ~children:[_exit] ()] ()
        ]
      ()
end

module Markup = Make (Tyxml.Xml) (Tyxml.Svg) (Tyxml.Html)
