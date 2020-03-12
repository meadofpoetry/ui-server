open Components_tyxml

let id = "https-config"

let enable_id = "https-enable"

module Make
    (Xml : Xml_sigs.NoWrap)
    (Svg : Svg_sigs.NoWrap with module Xml := Xml)
    (Html : Html_sigs.NoWrap with module Xml := Xml and module Svg := Svg) =
struct
  open Html

  open Button.Make (Xml) (Svg) (Html)

  open Card.Make (Xml) (Svg) (Html)

  open Form_field.Make (Xml) (Svg) (Html)

  open Switch.Make (Xml) (Svg) (Html)

  open Ui_templates_tyxml.Settings_page.Make (Xml) (Svg) (Html)

  let create ?classes ?(a = []) (v : Server_types.settings) : 'a elt =
    let submit =
      button ~classes:[ Card.CSS.action ] ~label:"Применить" ()
    in
    let enable_input_id = enable_id ^ "-input" in
    let switch = switch ~input_id:enable_input_id ~checked:v.https_enabled () in
    let enable =
      form_field ~a:[ a_id enable_id ]
        ~label:"Использовать HTTPS протокол"
        ~label_for:enable_input_id ~align_end:true ~input:switch ()
    in
    create_section ?classes ~a:(a_id id :: a)
      ~header:(create_section_header ~title:(`Text "HTTPS") ())
      ~children:
        [
          card_media ~children:[ enable ] ();
          card_actions
            ~children:[ card_action_buttons ~children:[ submit ] () ]
            ();
        ]
      ()
end

module F = Make (Tyxml.Xml) (Tyxml.Svg) (Tyxml.Html)
