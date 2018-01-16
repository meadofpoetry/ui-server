open Api.Template
open Api

let create () : upper item list =
  let props = { title        = None
              ; pre_scripts  = [ Src "/js/janus.nojquery.js"; Src "/js/adapter.min.js" ]
              ; post_scripts = [ Src "/js/pipeline.js" ]
              ; stylesheets  = []
              ; content      = []
              }
  in [Simple { title = "Pipeline"; href = Path.of_string "pipeline"; template = props }]
