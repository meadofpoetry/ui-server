open Pipeline_js

let () =
  let w = Wm_page.page () in
  let _ = new Ui_templates.Page.t (`Static [ w ]) () in
  ()

