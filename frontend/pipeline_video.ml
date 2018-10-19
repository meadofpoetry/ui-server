open Pipeline_js

let () =
  let w = Mosaic.page () in
  ignore @@ new Ui_templates.Page.t (`Static [ w ]) ()
