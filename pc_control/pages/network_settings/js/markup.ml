open Js_of_ocaml_tyxml

include Page_network_settings_tyxml

include Make(Tyxml_js.Xml)(Tyxml_js.Svg)(Tyxml_js.Html)
