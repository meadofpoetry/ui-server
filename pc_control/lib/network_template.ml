open Common.User
open Api.Template
open Common.Uri

module Icon = Components_tyxml.Icon.Make(Tyxml.Xml)(Tyxml.Svg)(Tyxml.Html)

let create () : upper ordered_item list user_table =
  let id = "network-settings" in
  let props =
    make_tmpl_props ~id
      ~app_bar:(make_app_bar_props ~title:"Сеть" ())
      ~post_scripts:[Src "/js/network.js"]
      () in
  let icon x =
    let open Icon.SVG in
    let path = create_path x () in
    let icon = create [path] () in
    Tyxml.Html.toelt icon in
  let network_pages =
    [`Index 10,
     Simple { id
            ; title = "Сеть"
            ; icon = Some (icon Icon.SVG.Path.lan)
            ; href = Path.of_string "network"
            ; template = props }] in
  { root =
      [ `Index 5,
        Subtree { title = "Настройки"
                ; icon = Some (icon Icon.SVG.Path.settings)
                ; href = Path.of_string "settings"
                ; templates = network_pages } ]
  ; operator = []
  ; guest = []
  }
