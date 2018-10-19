open Pipeline_js

let () =
  let w = Wm_page.page () in
  ignore @@ new Ui_templates.Page.t (`Static [ w ]) ()

