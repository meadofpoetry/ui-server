open Components_tyxml
open Components_lab_tyxml

module CSS = struct
  include Ui_templates_tyxml.Settings_page.CSS

  let root = "software-updates"

  let action = BEM.add_element root "action"

  let action_check = BEM.add_modifier action "check"

  let action_update = BEM.add_modifier action "update"
end

let unchecked_hint =
  "Нет данных о доступных обновлениях"

let not_available_hint =
  "Нет доступных обновлений"

let available_hint =
  "Обнаружены доступные обновления"

let reboot_hint =
  "Обновления успешно установлены.\nТребуется перезагрузка прибора."

let action_label_check_updates = "Проверить наличие обновлений"

let action_label_update = "Обновить"

let action_label_reboot = "Перезагрузить"

module Make(Xml : Xml_sigs.NoWrap)
    (Svg : Svg_sigs.NoWrap with module Xml := Xml)
    (Html : Html_sigs.NoWrap with module Xml := Xml
                              and module Svg := Svg) = struct
  module Progress = Linear_progress.Make(Xml)(Svg)(Html)

  module Button = Button.Make(Xml)(Svg)(Html)

  module Placeholder = Placeholder.Make(Xml)(Svg)(Html)

  module Icon = Icon.Make(Xml)(Svg)(Html)

  include Ui_templates_tyxml.Settings_page.Make(Xml)(Svg)(Html)

  let make_placeholder ?classes ?attrs () =
    let widget = Icon.SVG.(create [create_path Svg_icons.cloud_question ()] ()) in
    Placeholder.make_simple ?classes ?attrs widget
      unchecked_hint

  let make_update_section ?classes ?(attrs = []) () =
    make_section ?classes ~attrs:([Html.a_id "remote-update"] @ attrs)
      ~header:(make_section_header ~title:"Дистанционное обновление" [])
      [ Card'.create_media
          [ Progress.create ~closed:true ()
          ; make_placeholder ()
          ]
          ()
      ; Card'.create_actions
          [ Button.create
              ~appearance:Raised
              ~label:action_label_check_updates
              ()
          ]
          ()
      ]
end
