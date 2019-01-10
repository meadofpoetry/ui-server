open Js_of_ocaml
open Components

let log (x : 'a) : unit =
  Js.Unsafe.global##.console##log x

let pagination () =
  new Pagination.t ~visible_pages:5 ~total:10 ()

let onload _ =
  let page = new Ui_templates.Page.t (`Static []) () in
  let div = pagination () in
  page#arbitrary#append_child div;
  Js._false

let () =
  Dom_html.addEventListener Dom_html.document
    Dom_events.Typ.domContentLoaded
    (Dom_html.handler onload)
    Js._false
  |> ignore
