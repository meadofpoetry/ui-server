open Components_tyxml

module Cert_viewer = Cert_viewer

module CSS = struct
  include Ui_templates_tyxml.Settings_page.CSS
  let file_button = "file-button"

  module Certificate = struct
    let root = "certificate-config"
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
end

module Make(Xml : Xml_sigs.NoWrap)
    (Svg : Svg_sigs.NoWrap with module Xml := Xml)
    (Html : Html_sigs.NoWrap with module Xml := Xml
                              and module Svg := Svg) = struct
  module Components = Components_tyxml.Bundle.Make(Xml)(Svg)(Html)
  include Ui_templates_tyxml.Settings_page.Make(Xml)(Svg)(Html)

  open Html
  open Components

  let make_icon_button icon =
    let path = Icon.SVG.create_path icon () in
    Icon_button.create ~icon:Icon.SVG.(create [path] ())

  module HTTPS = struct

    let id = "https-config"

    let enable_id = "https-enable"

    let make ?classes ?(attrs = []) (v : Server_types.settings) : 'a elt =
      let submit = Button.create
          ~classes:[Card.CSS.action]
          ~label:"Применить"
          () in
      let enable_input_id = enable_id ^ "-input" in
      let switch = Switch.create
          ~input_id:enable_input_id
          ~checked:v.https_enabled
          () in
      let enable =
        Form_field.create ~attrs:[a_id enable_id]
          ~label:(Form_field.create_label
                    ~for_id:enable_input_id
                    "Использовать HTTPS протокол"
                    ())
          ~align_end:true
          ~input:switch
          () in
      make_section ?classes ~attrs:(a_id id :: attrs)
        ~header:(make_section_header ~title:"HTTPS" [])
        [ Card.create_media [enable] ()
        ; Card.create_actions [Card.create_action_buttons [submit] ()] ()
        ]
  end

  module Certificate = struct
    let id = "certificate-config"

    let empty = "Не установлен"

    let make_import_button ?(classes = []) ?(attrs = []) ~label () =
      let classes = CSS.file_button :: classes in
      let input = input ~a:[a_input_type `File] () in
      let button = Button.create ~label () in
      div ~a:([a_class classes] @ attrs) [input; button]

    let make_row ?(classes = []) ?(attrs = []) ?name typ =
      let ( ^:: ) x l = match x with None -> l | Some x -> x :: l in
      let state = match name with
        | None -> CSS.Certificate.empty
        | Some _ -> "" in
      let name = match name with
        | Some n -> n
        | None -> empty in
      let class_, icon, title = match typ with
        | `Crt -> CSS.Certificate.certificate,
                  Svg_icons.certificate,
                  "Сертификат"
        | `Key -> CSS.Certificate.private_key,
                  Svg_icons.key,
                  "Приватный ключ" in
      let classes = CSS.Certificate.row :: state :: class_ :: classes in
      let path = Icon.SVG.create_path icon () in
      let icon = Icon.SVG.create [path] () in
      let import = make_import_button
          ~classes:[CSS.Certificate.action; CSS.Certificate.action_import]
          ~label:"Импорт"
          () in
      let remove = make_icon_button
          ~classes:[CSS.Certificate.action; CSS.Certificate.action_remove]
          Svg_icons.delete
          () in
      let info = match typ with
        | `Key -> None
        | `Crt -> Some (
            make_icon_button
              ~classes:[CSS.Certificate.action; CSS.Certificate.action_info]
              Svg_icons.information
              ()) in
      div ~a:([a_class classes] @ attrs)
        [ span [icon; txt title]
        ; span ~a:[a_class [CSS.Certificate.filename]] [txt name]
        ; div ~a:[a_class [CSS.Certificate.actions]]
            (import :: (info ^:: (remove :: [])))
        ]

    let make ?classes ?(attrs = []) (v : Server_types.settings) : 'a elt =
      make_section ?classes ~attrs:(a_id id :: attrs)
        ~header:(make_section_header ~title:"Сертификат" [])
        [Card.create_media ~classes:[CSS.Certificate.root]
           [ make_row ?name:(match v.tls_cert with
                 | None -> None | Some (n, _) -> Some n) `Crt
           ; make_row ?name:v.tls_key `Key]
           ()]
  end

end
