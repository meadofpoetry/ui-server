open Js_of_ocaml
open Components

let () =
  let scaffold = Scaffold.attach (Dom_html.getElementById "root") in
  ignore scaffold
