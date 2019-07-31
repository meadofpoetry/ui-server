open Js_of_ocaml
open Components

let () =
  let scaffold = Scaffold.attach (Dom_html.getElementById "root") in
  let password = Password.make () in
  let page =
    Widget.create
    @@ Js_of_ocaml_tyxml.Tyxml_js.To_dom.of_element
    @@ Markup.make [password#markup] in
  scaffold#set_body page
