open Api.Template
open Api

let create () : upper ordered_item list =
  let props = { title        = None
              ; pre_scripts  = [ Src "/js/janus.nojquery.js"; Src "/js/adapter.min.js" ]
              ; post_scripts = [ Src "/js/user.js" ]
              ; stylesheets  = []
              ; content      = []
              }
  in
  let user_pages = [`Index 10, Simple { title = "Пользователи"; href = Path.of_string "user"; template = props }]
  in [`Index 5, Subtree { title = "Настройки"; href = Path.of_string "settings"; templates = user_pages };
      `Index 10, Ref { title = "Выход"; absolute = false; href = Path.of_string "api/user/logout"} ]
