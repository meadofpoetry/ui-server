open Common.User
open Api.Template
open Api

let home_template () : upper ordered_item list user_table =
  let props =
    { id = None
    ; title = None
    ; pre_scripts = []
    ; post_scripts = [Src "/js/home.js"]
    ; stylesheets = []
    ; content = []
    } in
  let rval = [`None, Home props] in
  { root = rval
  ; operator = rval
  ; guest = rval
  }
