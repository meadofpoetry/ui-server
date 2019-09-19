open Components_tyxml
open Application_types

module CSS = struct
  let root = "password-section"

  let slider = "slider"

  let text_field_container = "text-field-container"
end

module Make
    (Xml : Xml_sigs.NoWrap)
    (Svg : Svg_sigs.NoWrap with module Xml := Xml)
    (Html : Html_sigs.NoWrap with module Xml := Xml and module Svg := Svg) =
struct
  open Html

  open Ui_templates_tyxml.Settings_page.Make (Xml) (Svg) (Html)

  module Components = Bundle.Make (Xml) (Svg) (Html)

  let id = "password-config"

  let form_id user = "password-config-form-" ^ User.to_string user

  let old_pass_id user = "old-password-" ^ User.to_string user

  let new_pass_id user = "new-password-" ^ User.to_string user

  let confirm_pass_id user = "confirm-password-" ^ User.to_string user

  let create_textfield ?(autocomplete = "off") ?value ~name ~label ~id () : 'a elt =
    let id' = id ^ "-input" in
    let icon =
      div
        ~a:[a_class [Textfield.CSS.icon]; a_role ["button"]; a_tabindex 0]
        [Components.Icon.SVG.icon ~d:Svg_icons.eye_off ()]
    in
    let input =
      Unsafe.coerce_elt
      @@ Components.Textfield.create_input
           ~id:id'
           ~attrs:
             [ Unsafe.string_attrib "autocomplete" autocomplete
             ; Unsafe.string_attrib "spellcheck" "false"
             ; a_name name ]
           ~typ:`Password
           ?value
           ()
    in
    Components.Textfield.create
      ~attrs:[a_id id]
      ~trailing_icon:icon
      ~input_id:id'
      ~input
      ~label:(`Text label)
      ()

  let create_user_tabs () =
    let create_tab ?active user =
      let icon =
        Components.Icon.SVG.(
          icon ~classes:[Tab.CSS.icon] ~d:(Util.user_icon_path user) ())
      in
      let username = Application_types.User.to_string user in
      let username_human = Format.asprintf "%a" Util.pp_user_human user in
      Components.Tab.create
        ?active
        ~icon
        ~text_label:(`Text username_human)
        ~attrs:[a_user_data "username" username]
        ()
    in
    Components.Tab_bar.create
      ~tabs:[create_tab ~active:true `Guest; create_tab `Operator; create_tab `Root]
      ()

  let create_username user =
    Unsafe.coerce_elt
    @@ input
         ~a:
           [ Unsafe.string_attrib "autocomplete" "username"
           ; a_input_type `Text
           ; a_name "username"
           ; a_value user
           ; a_aria "hidden" ["true"] ]
         ()

  let create_helper_text ?(persistent = true) text =
    Components.Textfield.(
      create_helper_line
        ~children:[Helper_text.create ~validation:true ~persistent ~text ()]
        ())

  let create_form user =
    let text_field_container content =
      div ~a:[a_class [CSS.text_field_container]] content
    in
    let username = Application_types.User.to_string user in
    form
      ~a:[a_user_data "username" username]
      [ create_username username
      ; text_field_container
          [ create_textfield
              ~id:(old_pass_id user)
              ~name:"current_password"
              ~autocomplete:"current-password"
              ~label:"Старый пароль"
              ()
          ; create_helper_text ~persistent:false "" ]
      ; text_field_container
          [ create_textfield
              ~id:(new_pass_id user)
              ~name:"new_password"
              ~autocomplete:"new-password"
              ~label:"Новый пароль"
              ()
          ; create_helper_text "Минимум 4 символа" ]
      ; text_field_container
          [ create_textfield
              ~id:(confirm_pass_id user)
              ~name:"confirm_password"
              ~autocomplete:"new-password"
              ~label:"Подтвердите новый пароль"
              ()
          ; create_helper_text ~persistent:false "" ]
      ; input ~a:[a_input_type `Submit] () ]

  let create ?(classes = []) ?(attrs = []) () =
    let submit =
      Components.Button.button
        ~classes:[Card.CSS.action]
        ~appearance:Raised
        ~button_type:`Submit
        ~label:"Сменить пароль"
        ()
    in
    let classes = CSS.root :: classes in
    create_section
      ~classes
      ~attrs:(a_id id :: attrs)
      ~header:(create_section_header ~title:(`Text "Пароли") ())
      ~children:
        [ create_user_tabs ()
        ; hr ()
        ; Components.Card.card_media
            ~children:
              [ div
                  ~a:[a_class [CSS.slider]]
                  [create_form `Guest; create_form `Operator; create_form `Root] ]
            ()
        ; Components.Card.card_actions
            ~children:[Components.Card.card_action_buttons ~children:[submit] ()]
            () ]
      ()
end

module Markup = Make (Tyxml.Xml) (Tyxml.Svg) (Tyxml.Html)
