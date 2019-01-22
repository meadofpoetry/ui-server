open Common.User
open Api.Template
open Common.Uri

module Icon = Components_markup.Icon.Make(Tyxml.Xml)(Tyxml.Svg)(Tyxml.Html)

let create () : upper ordered_item list user_table =
  let props = { title        = Some "Пользователи"
              ; pre_scripts  = []
              ; post_scripts = [ Src "/js/user.js" ]
              ; stylesheets  = [ "/css/user.min.css" ]
              ; content      = []
              } in
  let icon x =
    let open Icon.SVG in
    let path = create_path x () in
    let icon = create [path] () in
    Tyxml.Html.toelt icon in
  let user_pages =
    [`Index 10,
     Simple { title    = "Пользователи"
            ; icon     = Some (icon Icon.SVG.Path.account_settings_variant)
            ; href     = Path.of_string "user"
            ; template = props }] in
  { root = [`Index 5,
            Subtree { title     = "Настройки"
                    ; icon      = Some (icon Icon.SVG.Path.settings)
                    ; href      = Path.of_string "settings"
                    ; templates = user_pages } ]
  ; operator = []
  ; guest    = []
  }
