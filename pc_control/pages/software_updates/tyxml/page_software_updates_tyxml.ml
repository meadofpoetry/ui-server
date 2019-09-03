open Components_tyxml

module CSS = struct
  include Ui_templates_tyxml.Settings_page.CSS
end

module Make(Xml : Xml_sigs.NoWrap)
    (Svg : Svg_sigs.NoWrap with module Xml := Xml)
    (Html : Html_sigs.NoWrap with module Xml := Xml
                              and module Svg := Svg) = struct
  module Progress = Linear_progress.Make(Xml)(Svg)(Html)

  module Button = Button.Make(Xml)(Svg)(Html)

  include Ui_templates_tyxml.Settings_page.Make(Xml)(Svg)(Html)

  let make_update_section ?classes ?attrs () =
    make_section ?classes ?attrs
      ~header:(make_section_header ~title:"Дистанционное обновление" [])
      [ Progress.create ~closed:true ()
      ; Button.create
          ~appearance:Raised
          ~label:"Проверить наличие обновлений"
          ()
      ]
end
