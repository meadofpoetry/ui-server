open Netlib.Uri

module Api_template = Api_cohttp_template.Make (Application_types.User)
   
module Icon = Components_markup.Icon.Make(Tyxml.Xml)(Tyxml.Svg)(Tyxml.Html)

let create () =
  let open Api_template in
  let props =
    { title = Some "Сеть"
    ; pre_scripts = []
    ; post_scripts = [ Src "/js/network.js" ]
    ; stylesheets = []
    ; content = []
    }
  in
  let icon x =
    let open Icon.SVG in
    let path = create_path x () in
    let icon = create [path] () in
    Tyxml.Html.toelt icon
  in
  simple
    ~restrict:[`Operator; `Guest]
    ~priority:(`Index 10)
    ~title:"Сеть"
    ~icon:(icon Icon.SVG.Path.lan)
    ~path:(Path.of_string "network")
    props
