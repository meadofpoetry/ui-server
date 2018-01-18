open Common.User
open Api.Template
open Api

let create () : upper ordered_item list user_table =
  let props = { title        = None
              ; pre_scripts  = [ Src "/js/janus.nojquery.js"; Src "/js/adapter.min.js" ]
              ; post_scripts = [ Src "/js/user.js" ]
              ; stylesheets  = []
              ; content      = []
              }
  in
  let user_pages = [`Index 10, Simple { title = "Пользователи"; href = Path.of_string "user"; template = props }]
  in { root = [`Index 5, Subtree { title = "Настройки"; href = Path.of_string "settings"; templates = user_pages } ]
     ; operator = []
     ; guest    = []
     }
