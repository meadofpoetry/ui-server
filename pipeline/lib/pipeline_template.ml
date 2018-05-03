open Common.User
open Api.Template
open Api

let create () : upper ordered_item list user_table =
  let props = { title        = None
              ; pre_scripts  = [ Src "/js/janus.nojquery.js"; Src "/js/adapter.min.js";
                                 Src "/js/moment.min.js"; Src "/js/Chart.min.js"]
              ; post_scripts = [ Src "/js/pipeline.js" ]
              ; stylesheets  = [ "/css/pipeline.css" ]
              ; content      = []
              }
  in
  let rval = [`Index 1, Simple { title = "Мозаика"; href = Path.of_string "pipeline"; template = props }] in 
  { root = rval
  ; operator = rval
  ; guest = rval
  }
