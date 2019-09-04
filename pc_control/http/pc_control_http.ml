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
      ~post_scripts:[`Src "/js/page-network-settings.js"]
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

let software_updates_handlers (su : Pc_control.Software_updates.t) =
  let open Api_http in
  make ~prefix:"updates"
    [ node ~doc:"Check for available packages"
        ~restrict:[ `Guest; `Operator ]
        ~meth:`POST
        ~path:Path.Format.("check-updates" @/ empty)
        ~query:Query.empty
        (Pc_control.Software_updates_api.check_for_upgrades su)
    ; node ~doc:"Upgrade"
        ~restrict:[ `Guest; `Operator ]
        ~meth:`POST
        ~path:Path.Format.("upgrade" @/ empty)
        ~query:Query.["reboot", (module Option(Bool))]
        (Pc_control.Software_updates_api.do_upgrade su)
    ; node ~doc:"Current state"
        ~meth:`GET
        ~path:Path.Format.("state" @/ empty)
        ~query:Query.empty
        (Pc_control.Software_updates_api.get_state su)
    ]

let software_updates_ws (su : Pc_control.Software_updates.t) =
  let open Api_websocket in
  make ~prefix:"updates"
    [ event_node ~doc:"Update state"
        ~path:Path.Format.("state" @/ empty)
        ~query:Query.empty
        (Pc_control.Software_updates_api.Event.get_state su)
    ]

let software_updates_pages : 'a. unit -> 'a Api_template.item list =
  fun () ->
  let open Api_template in
  let props =
    make_template_props
      ~title:"Обновления"
      ~post_scripts:[`Src "/js/page-software-updates.js"]
      ~stylesheets:["/css/page-software-updates.min.css"]
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
    ~title:"Обновления"
    ~icon:(icon Components_tyxml.Svg_icons.update)
    ~path:(Path.of_string "settings/updates")
    props

let power_handlers =
  let open Api_http in
  make ~prefix:"power"
    [ node ~doc:"Reboot"
        ~restrict:[ `Guest; `Operator ]
        ~meth:`POST
        ~path:Path.Format.("reboot" @/ empty)
        ~query:Query.empty
        Pc_control.Power_api.reboot
    ; node ~doc:"Off"
        ~restrict:[ `Guest; `Operator ]
        ~meth:`POST
        ~path:Path.Format.("off" @/ empty)
        ~query:Query.empty
        Pc_control.Power_api.off
    ]

module Power_page_markup = struct
  include Page_power_management_tyxml.Make(Tyxml.Xml)(Tyxml.Svg)(Tyxml.Html)
end

let power_pages : 'a. unit -> 'a Api_template.item list =
  fun () ->
  let open Api_template in
  let markup = Tyxml.Html.toelt @@ Power_page_markup.make () in
  let props =
    make_template_props
      ~title:"Управление питанием"
      ~post_scripts:[`Src "/js/page-power-management.js"]
      ~stylesheets:["/css/page-power-management.min.css"]
      ~content:[markup]
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
    ~title:"Питание"
    ~icon:(icon Components_tyxml.Svg_icons.power)
    ~path:(Path.of_string "settings/power")
    props
