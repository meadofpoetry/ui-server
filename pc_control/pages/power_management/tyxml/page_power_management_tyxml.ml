module CSS = struct
  include Ui_templates_tyxml.Settings_page.CSS

  let root = "power-management"
end

module Reboot = Reboot
module Shutdown = Shutdown

module Make
    (Xml : Xml_sigs.NoWrap)
    (Svg : Svg_sigs.NoWrap with module Xml := Xml)
    (Html : Html_sigs.NoWrap with module Xml := Xml and module Svg := Svg) =
struct
  module Shutdown_markup = Shutdown.Make (Xml) (Svg) (Html)
  module Reboot_markup = Reboot.Make (Xml) (Svg) (Html)

  open Ui_templates_tyxml.Settings_page.Make (Xml) (Svg) (Html)

  let create ?(classes = []) ?a () =
    let classes = CSS.root :: classes in
    create ~classes ?a
      ~children:[ Shutdown_markup.create (); Reboot_markup.create () ]
      ()
end

module F = Make (Tyxml.Xml) (Tyxml.Svg) (Tyxml.Html)
