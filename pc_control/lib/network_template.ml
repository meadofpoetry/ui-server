open Common.User
open Api.Template
open Api

let create () : upper ordered_item list user_table =
  let props = { title        = None
              ; pre_scripts  = [ Src "/js/janus.nojquery.js"; Src "/js/adapter.min.js" ]
              ; post_scripts = [ Src "/js/network.js" ]
              ; stylesheets  = []
              ; content      = []
              }
  in
  let network_pages = [`Index 10, Simple { title = "Параметры сети"; href = Path.of_string "network"; template = props }]
  in { root = [`Index 5, Subtree { title = "Настройки"; href = Path.of_string "settings"; templates = network_pages } ]
     ; operator = []
     ; guest    = []
     } 
