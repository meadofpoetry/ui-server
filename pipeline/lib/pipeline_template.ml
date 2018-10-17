open Common.User
open Api.Template
open Api

module Icon = Components_markup.Icon.Make(Tyxml.Xml)(Tyxml.Svg)(Tyxml.Html)

let create () : upper ordered_item list user_table =
  let props = { title        = None
              ; pre_scripts  = [ Src "/js/janus.nojquery.js"
                               ; Src "/js/adapter.min.js"
                               ; Src "/js/moment.min.js"
                               ; Src "/js/Chart.min.js"]
              ; post_scripts = [ Src "/js/pipeline.js" ]
              ; stylesheets  = [ "/css/pipeline.min.css" ]
              ; content      = []
              }
  in
  let icon =
    let open Icon.SVG in
    let path = create_path Path.collage () in
    let icon = create [path] () in

    Tyxml.Html.toelt icon in
  let rval = [`Index 1, Simple { title    = "Мозаика"
                               ; icon     = Some icon
                               ; href     = Path.of_string "pipeline"
                               ; template = props }] in
  { root     = rval
  ; operator = rval
  ; guest    = rval
  }
