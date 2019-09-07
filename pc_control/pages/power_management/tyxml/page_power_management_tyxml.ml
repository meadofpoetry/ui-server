module CSS = struct
  include Ui_templates_tyxml.Settings_page.CSS

  let root = "power-management"
end

module Make
    (Xml : Xml_sigs.NoWrap)
    (Svg : Svg_sigs.NoWrap with module Xml := Xml)
    (Html : Html_sigs.NoWrap with module Xml := Xml and module Svg := Svg) =
struct
  module Shutdown_section = Shutdown.Make (Xml) (Svg) (Html)
  module Reboot_section = Reboot.Make (Xml) (Svg) (Html)

  open Ui_templates_tyxml.Settings_page.Make (Xml) (Svg) (Html)

  let make ?(classes = []) ?attrs () =
    let classes = CSS.root :: classes in
    make ~classes ?attrs [Shutdown_section.make (); Reboot_section.make ()]
end
