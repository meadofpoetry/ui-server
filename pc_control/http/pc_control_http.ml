open Application_types
open Netlib.Uri

module Api_http = Api_cohttp.Make (User) (Body)

module Api_template = Api_cohttp_template.Make (User)

module Icon = Components_tyxml.Icon.Make(Tyxml.Xml)(Tyxml.Svg)(Tyxml.Html)

let network_handlers (network : Pc_control.Network.t) =
  let open Api_http in
  make ~prefix:"network"
    [ node ~doc:"Network configuration"
        ~meth:`GET
        ~path:Path.Format.("config" @/ empty)
        ~query:Query.empty
        (Pc_control.Network_api.get_config network)
    ; node ~doc:"Network configuration"
        ~restrict:[ `Guest; `Operator ]
        ~meth:`POST
        ~path:Path.Format.("config" @/ empty)
        ~query:Query.empty
        (Pc_control.Network_api.set_config network)
    ]

let network_pages : Api_template.topmost Api_template.item list =
  let open Api_template in
  let props =
    make_template_props
      ~title:"Сеть"
      ~post_scripts:[Src "/js/network.js"]
      ()
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
    ~icon:(icon Components_tyxml.Svg_icons.lan)
    ~path:(Path.of_string "settings/network")
    props
