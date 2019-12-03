module CSS = struct
  include Ui_templates_tyxml.Settings_page.CSS
end

module Make
    (Xml : Xml_sigs.NoWrap)
    (Svg : Svg_sigs.NoWrap with module Xml := Xml)
    (Html : Html_sigs.NoWrap with module Xml := Xml and module Svg := Svg) =
struct
  include Ui_templates_tyxml.Settings_page.Make (Xml) (Svg) (Html)
end

module F = Make (Tyxml.Xml) (Tyxml.Svg) (Tyxml.Html)
