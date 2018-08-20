open Common.User
open Api.Template
open Api

module Icon = Components_markup.Icon.Make(Tyxml.Xml)(Tyxml.Svg)(Tyxml.Html)

let create () : upper ordered_item list user_table =
  let video =
    let template =
      { title = Some "Видео"
      ; pre_scripts  = [ Src "/js/janus.nojquery.js"
                       ; Src "/js/adapter.min.js"
                       ; Src "/js/moment.min.js"
                       ; Src "/js/Chart.min.js"]
      ; post_scripts = [ Src "/js/pipeline_video.js" ]
      ; stylesheets = [ "/css/pipeline.min.css" ]
      ; content = []
      } in
    let icon =
      let open Icon.SVG in
      let path = create_path Path.video () in
      let icon = create [ path ] () in
      Tyxml.Html.toelt icon in
    Simple { title = "Видео"
           ; icon = Some icon
           ; href = Path.of_string "video"
           ; template } in
  let editor =
    let template =
      { title = Some "Редактор"
      ; pre_scripts = [ ]
      ; post_scripts = [ Src "/js/pipeline_editor.js" ]
      ; stylesheets = [ "/css/pipeline.min.css" ]
      ; content = [ ]
      } in
    let icon =
      let open Icon.SVG in
      let path = create_path Path.table_edit () in
      let icon = create [ path ] () in
      Tyxml.Html.toelt icon in
    Simple { title = "Редактор"
           ; icon = Some icon
           ; href = Path.of_string "editor"
           ; template } in
  let icon =
    let open Icon.SVG in
    let path = create_path Path.collage () in
    let icon = create [path] () in

    Tyxml.Html.toelt icon in
  let templates = [ `Index 1, video
                  ; `Index 2, editor ] in
  let rval =
    [`Index 1, Subtree { title = "Мозаика"
                       ; icon = Some icon
                       ; href = Path.of_string "pipeline"
                       ; templates }]
  in
  { root     = rval
  ; operator = rval
  ; guest    = rval
  }
