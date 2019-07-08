open Js_of_ocaml_tyxml.Tyxml_js

module M = Page_mosaic_editor_tyxml

include M.Make(Xml)(Svg)(Html)

module CSS = M.CSS
module Container_editor = M.Container_editor.Make(Xml)(Svg)(Html)
