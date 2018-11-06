open Containers
open Components
open Common
open Application_js.Page_sandbox

let () =
  let open Common.Stream in
  let w = make () in
  let page = new Ui_templates.Page.t (`Static [w]) () in
  ()
