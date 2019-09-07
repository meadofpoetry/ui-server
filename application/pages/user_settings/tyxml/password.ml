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
        [Components.Icon.SVG.create_of_d Svg_icons.eye_off]
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
    let label =
      Components.Floating_label.create
        ~classes:
          (match value with
          | None -> []
          | Some _ -> [Floating_label.CSS.float_above])
        ~for_:id'
        label
        ()
    in
    let line_ripple = Components.Line_ripple.create () in
    Components.Textfield.create
      ~attrs:[a_id id]
      ~trailing_icon:icon
      ~input
      ~label
      ~line_ripple
      ()

  let create_user_tabs () =
    let create_tab ?active user =
      let icon =
        Components.Icon.SVG.(
          create_of_d ~classes:[Tab.CSS.icon] (Util.user_icon_path user))
      in
      let username = Application_types.User.to_string user in
      let indicator =
        Components.Tab_indicator.create
          ?active
          (Components.Tab_indicator.create_content ())
          ()
      in
      let username_human = Format.asprintf "%a" Util.pp_user_human user in
      let text_label = Components.Tab.create_text_label username_human () in
      Components.Tab.create
        ?active
        ~attrs:[a_user_data "username" username]
        ~indicator
        (Components.Tab.create_content ~text_label ~icon ())
        ()
    in
    let tabs =
      [create_tab ~active:true `Guest; create_tab `Operator; create_tab `Root]
    in
    let scroll_area =
      Components.Tab_scroller.create_scroll_area
        ~content:(Components.Tab_scroller.create_scroll_content tabs ())
        ()
    in
    let scroller = Components.Tab_scroller.create ~scroll_area () in
    Components.Tab_bar.create ~scroller ()

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
    Components.Textfield.create_helper_line
      [Components.Textfield.Helper_text.create ~validation:true ~persistent ~text ()]

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
      Components.Button.create
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
      ~header:(create_section_header ~title:"Пароли" [])
      [ create_user_tabs ()
      ; hr ()
      ; Components.Card.create_media
          [ div
              ~a:[a_class [CSS.slider]]
              [create_form `Guest; create_form `Operator; create_form `Root] ]
      ; Components.Card.create_actions [Components.Card.create_action_buttons [submit]]
      ]
end

module Markup = Make (Tyxml.Xml) (Tyxml.Svg) (Tyxml.Html)
