open Pipeline_js

let () =
  let w = Mosaic.page () in
  let _ = new Ui_templates.Page.t (`Static [ w ]) () in
  ()
