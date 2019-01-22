open Common.User
open Api.Template
   
let home_template () : upper ordered_item list user_table =
  let content = Tyxml.Html.(
      div [ p [txt "This is the main ATS-3 page"] ]
      |> (Format.asprintf "%a" (pp_elt ()))) in
  let props =
    { title = Some "ATS-3 home page"
    ; pre_scripts = [ Src "/js/janus.nojquery.js"; Src "/js/adapter.min.js" ]
    ; post_scripts = [ Src "/js/home.js" ]
    ; stylesheets = []
    ; content = [content]
    } in
  let rval = [`None, Home props] in
  { root = rval
  ; operator = rval
  ; guest = rval
  }
