open Js_of_ocaml

let () =
  let (scaffold : Components.Scaffold.t) = Js.Unsafe.global##.scaffold in
  let thread = Ui_templates.Tabbed_page.init () in
  let _loader =
    Components_lab.Loader.make_loader
      ~on_success:(fun _ x -> scaffold#set_on_destroy x#finalize)
      ~elt:scaffold#app_content_inner
      thread
  in
  ()
