open Application_types
open Netlib.Uri

module Api_http = Api_cohttp.Make(User)(Body)

module Api_template = Api_cohttp_template.Make(User)

module Api_websocket = Api_websocket.Make(User)(Body)(Body_ws)

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

let network_ws (network : Pc_control.Network.t) =
  let open Api_websocket in
  make ~prefix:"network"
    [ event_node ~doc:"Network configuration"
        ~path:Path.Format.("config" @/ empty)
        ~query:Query.empty
        (Pc_control.Network_api.Event.get_config network)
    ]

let network_pages : 'a. unit -> 'a Api_template.item list =
  fun () ->
  let open Api_template in
  let props =
    make_template_props
      ~title:"Сетевые настройки"
      ~post_scripts:[Src "/js/page-network-settings.js"]
      ~stylesheets:["/css/page-network-settings.min.css"]
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
