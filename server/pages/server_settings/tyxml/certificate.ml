open Components_tyxml

let id = "certificate-config"

let empty = "Не установлен"

module CSS = struct
  let root = "certificate-config"

  let file_button = "file-button"

  let row = root ^ "-row"

  let certificate = BEM.add_modifier row "certificate"

  let private_key = BEM.add_modifier row "private-key"

  let empty = BEM.add_modifier row "empty"

  let actions = BEM.add_element row "actions"

  let filename = BEM.add_element row "filename"

  let action = BEM.add_element row "action"

  let action_info = BEM.add_modifier action "info"

  let action_remove = BEM.add_modifier action "remove"

  let action_import = BEM.add_modifier action "import"
end

module Make
    (Xml : Xml_sigs.NoWrap)
    (Svg : Svg_sigs.NoWrap with module Xml := Xml)
    (Html : Html_sigs.NoWrap with module Xml := Xml and module Svg := Svg) =
struct
  open Html

  open Button.Make (Xml) (Svg) (Html)

  open Card.Make (Xml) (Svg) (Html)

  open Icon.Make (Xml) (Svg) (Html)

  open Icon_button.Make (Xml) (Svg) (Html)

  open Ui_templates_tyxml.Settings_page.Make (Xml) (Svg) (Html)

  let create_import_button ?(classes = []) ?(a = []) ~label () =
    let classes = CSS.file_button :: classes in
    let input = input ~a:[a_input_type `File] () in
    let button = button ~label () in
    div ~a:([a_class classes] @ a) [input; button]

  let create_row ?(classes = []) ?(a = []) ?name typ =
    let ( ^:: ) x l =
      match x with
      | None -> l
      | Some x -> x :: l
    in
    let state =
      match name with
      | None -> CSS.empty
      | Some _ -> ""
    in
    let name =
      match name with
      | Some n -> n
      | None -> empty
    in
    let class_, icon, title =
      match typ with
      | `Crt -> CSS.certificate, Svg_icons.certificate, "Сертификат"
      | `Key -> CSS.private_key, Svg_icons.key, "Приватный ключ"
    in
    let classes = CSS.row :: state :: class_ :: classes in
    let import =
      create_import_button
        ~classes:[CSS.action; CSS.action_import]
        ~label:"Импорт"
        ()
    in
    let remove =
      icon_button
        ~classes:[CSS.action; CSS.action_remove]
        ~icon:(SVG.icon ~d:Svg_icons.delete ())
        ()
    in
    let info =
      match typ with
      | `Key -> None
      | `Crt ->
          Some
            (icon_button
               ~classes:[CSS.action; CSS.action_info]
               ~icon:(SVG.icon ~d:Svg_icons.information ())
               ())
    in
    div
      ~a:([a_class classes] @ a)
      [ span [SVG.icon ~d:icon (); txt title]
      ; span ~a:[a_class [CSS.filename]] [txt name]
      ; div ~a:[a_class [CSS.actions]] (import :: (info ^:: [remove])) ]

  let create ?classes ?(a = []) (v : Server_types.settings) : 'a elt =
    create_section
      ?classes
      ~a:(a_id id :: a)
      ~header:(create_section_header ~title:(`Text "Сертификат") ())
      ~children:
        [ Card_markup.card_media
            ~classes:[CSS.root]
            ~children:
              [ create_row
                  ?name:
                    (match v.tls_cert with
                    | None -> None
                    | Some (n, _) -> Some n)
                  `Crt
              ; create_row ?name:v.tls_key `Key ]
            () ]
      ()
end

module F = Make (Tyxml.Xml) (Tyxml.Svg) (Tyxml.Html)
