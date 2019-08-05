open Components

let () =
  let scaffold = Scaffold.attach (Js_of_ocaml.Dom_html.getElementById "root") in
  Js_of_ocaml.Js.Unsafe.global##.scaffold := scaffold
