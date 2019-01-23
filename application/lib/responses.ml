open Common.User
open Api.Template

let home_template () : upper ordered_item list user_table =
  let props = make_tmpl_props ~post_scripts:[Src "/js/home.js"] () in
  let rval = [`None, Home props] in
  { root = rval
  ; operator = rval
  ; guest = rval
  }
