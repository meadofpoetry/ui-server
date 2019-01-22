open Common.User
open Api.Template
open Common.Uri

module Icon = Components_markup.Icon.Make(Tyxml.Xml)(Tyxml.Svg)(Tyxml.Html)

let make_icon path =
  let open Icon.SVG in
  let path = create_path path () in
  let icon = create [path] () in
  Tyxml.Html.toelt icon

let create () : upper ordered_item list user_table =
  let template =
    { title = Some "Мозаика"
    ; pre_scripts = [ Src "/js/janus.nojquery.js"
                    ; Src "/js/adapter.min.js"
                    ]
    ; post_scripts = [Src "/js/page_mosaic.js"]
    ; stylesheets = ["/css/pipeline.min.css"]
    ; content = []
    } in
  let rval =
    [`Index 1,
     Simple { title = "Мозаика"
            ; icon = Some (make_icon Icon.SVG.Path.collage)
            ; href = Path.of_string "pipeline"
            ; template }
    ]
  in
  { root = rval; operator = rval; guest = rval
  }
