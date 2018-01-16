open Api.Template
open Api
   
let home_template () : upper item list =
  let content = Tyxml.Html.(div [ p [pcdata "This is the main ATS-3 page"] ]
                            |> (Format.asprintf "%a" (pp_elt ())))
  in
  let props = { title        = Some "ATS-3 home page"
              ; pre_scripts  = [ Src "/js/janus.nojquery.js"; Src "/js/adapter.min.js" ]
              ; post_scripts = [ Src "/js/home.js" ]
              ; stylesheets  = []
              ; content      = [content]
              }
  in [Home props]
