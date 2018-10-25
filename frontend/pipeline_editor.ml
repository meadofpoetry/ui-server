open Pipeline_js

let () =
  let w = Page_editor.page () in
  ignore @@ new Ui_templates.Page.t (`Static [ w ]) ()

