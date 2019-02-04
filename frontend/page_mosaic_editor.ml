open Js_of_ocaml
open Pipeline_js
open Components

let () =
  let scaffold = Scaffold.attach (Dom_html.getElementById "root") in
  let body = Page_editor.page () in
  scaffold#set_body body
