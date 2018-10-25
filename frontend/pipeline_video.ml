open Pipeline_js

let () =
  let w = Page_video.page () in
  ignore @@ new Ui_templates.Page.t (`Static [ w ]) ()
