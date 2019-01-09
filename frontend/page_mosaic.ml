open Pipeline_js
open Components

let tabs =
  let data =
    [ "Видео", "video", (fun () -> Page_video.page ())
    ; "Редактор", "editor", (fun () -> Page_editor.page ())
    ] in
  List.map (fun (name, hash, f) ->
      new Tab.t ~value:(hash, f) ~content:(Text name) ())
    data

let () =
  ignore @@ new Ui_templates.Page.t (`Dynamic tabs) ()
