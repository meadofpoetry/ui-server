open Js_of_ocaml_tyxml

include Page_server_settings_tyxml

include Make(Tyxml_js.Xml)(Tyxml_js.Svg)(Tyxml_js.Html)

module Certificate_viewer = Cert_viewer.Make(Tyxml_js.Xml)(Tyxml_js.Svg)(Tyxml_js.Html)
