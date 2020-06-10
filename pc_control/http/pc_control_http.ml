open Application_types
open Netlib.Uri
module Api_http = Api_cohttp.Make (User) (Body)
module Api_template = Api_cohttp_template.Make (User)
module Api_websocket = Api_websocket.Make (User) (Body) (Body_ws)

let icon d =
  let icon = Components_tyxml.Icon.F.SVG.icon ~d () in
  Tyxml.Html.toelt icon

let network_handlers (network : Pc_control.Network.t) =
  let open Api_http in
  make ~prefix:"network"
    [
      node ~doc:"Network configuration" ~meth:`GET
        ~path:Path.Format.("config" @/ empty)
        ~query:Query.empty
        (Pc_control.Network_api.get_config network);
      node ~doc:"Network configuration" ~restrict:[ `Guest; `Operator ]
        ~meth:`POST
        ~path:Path.Format.("config" @/ empty)
        ~query:Query.empty
        (Pc_control.Network_api.set_config network);
    ]

let network_ws (network : Pc_control.Network.t) =
  let open Api_websocket in
  make ~prefix:"network"
    [
      event_node ~doc:"Network configuration"
        ~path:Path.Format.("config" @/ empty)
        ~query:Query.empty
        (Pc_control.Network_api.Event.get_config network);
    ]

let network_pages : 'a. unit -> 'a Api_template.item list =
 fun () ->
  let open Api_template in
  let props =
    make_template_props ~title:"Сетевые настройки"
      ~post_scripts:[ `Src "/js/page-network-settings.js" ]
      ~stylesheets:[ "/css/page-network-settings.min.css" ]
      ()
  in
  simple ~restrict:[ `Operator; `Guest ] ~priority:(`Index 10) ~title:"Сеть"
    ~icon:(icon Components_tyxml.Svg_icons.lan)
    ~path:(Path.of_string "settings/network")
    props

let software_updates_handlers (su : Pc_control.Software_updates.t) =
  let open Api_http in
  make ~prefix:"updates"
    [
      node ~doc:"Check for available packages" ~restrict:[ `Guest; `Operator ]
        ~meth:`POST
        ~path:Path.Format.("check-updates" @/ empty)
        ~query:Query.empty
        (Pc_control.Software_updates_api.check_for_upgrades su);
      node ~doc:"Upgrade" ~restrict:[ `Guest; `Operator ] ~meth:`POST
        ~path:Path.Format.("upgrade" @/ empty)
        ~query:Query.[ ("reboot", (module Option (Bool))) ]
        (Pc_control.Software_updates_api.do_upgrade su);
      node ~doc:"Current state" ~meth:`GET
        ~path:Path.Format.("state" @/ empty)
        ~query:Query.empty
        (Pc_control.Software_updates_api.get_state su);
    ]

let software_updates_ws (su : Pc_control.Software_updates.t) =
  let open Api_websocket in
  make ~prefix:"updates"
    [
      event_node ~doc:"Update state"
        ~path:Path.Format.("state" @/ empty)
        ~query:Query.empty
        (Pc_control.Software_updates_api.Event.get_state su);
    ]

let software_updates_pages : 'a. unit -> 'a Api_template.item list =
 fun () ->
  let open Api_template in
  let props =
    make_template_props ~title:"Обновления"
      ~post_scripts:[ `Src "/js/page-software-updates.js" ]
      ~stylesheets:[ "/css/page-software-updates.min.css" ]
      ()
  in
  simple ~restrict:[ `Operator; `Guest ] ~priority:(`Index 10)
    ~title:"Обновления"
    ~icon:(icon Components_tyxml.Svg_icons.update)
    ~path:(Path.of_string "settings/updates")
    props

let power_handlers =
  let open Api_http in
  make ~prefix:"power"
    [
      node ~doc:"Reboot" ~restrict:[ `Guest; `Operator ] ~meth:`POST
        ~path:Path.Format.("reboot" @/ empty)
        ~query:Query.empty Pc_control.Power_api.reboot;
      node ~doc:"Off" ~restrict:[ `Guest; `Operator ] ~meth:`POST
        ~path:Path.Format.("off" @/ empty)
        ~query:Query.empty Pc_control.Power_api.off;
    ]

let power_pages : 'a. unit -> 'a Api_template.item list =
 fun () ->
  let open Api_template in
  let markup = Tyxml.Html.toelt @@ Page_power_management_tyxml.F.create () in
  let props =
    make_template_props ~title:"Управление питанием"
      ~post_scripts:[ `Src "/js/page-power-management.js" ]
      ~stylesheets:[ "/css/page-power-management.min.css" ]
      ~content:[ markup ] ()
  in
  simple ~restrict:[ `Operator; `Guest ] ~priority:(`Index 10)
    ~title:"Питание"
    ~icon:(icon Components_tyxml.Svg_icons.power)
    ~path:(Path.of_string "settings/power")
    props

let time_handlers (t : Pc_control.Timedate.t) =
  let open Api_http in
  make ~prefix:"timedate"
    [
      node ~doc:"Timedate config" ~restrict:[ `Guest; `Operator ] ~meth:`GET
        ~path:Path.Format.("config" @/ empty)
        ~query:Query.empty
        (Pc_control.Timedate_api.get_config t);
      node ~doc:"List timezones" ~restrict:[ `Guest; `Operator ] ~meth:`GET
        ~path:Path.Format.("timezones" @/ empty)
        ~query:Query.empty
        (Pc_control.Timedate_api.get_timezones t);
      node ~doc:"Set timezone" ~restrict:[ `Guest; `Operator ] ~meth:`POST
        ~path:Path.Format.("timezone" @/ empty)
        ~query:Query.[ ("value", (module Single (String))) ]
        (Pc_control.Timedate_api.set_timezone t);
      node ~doc:"Set ntp" ~restrict:[ `Guest; `Operator ] ~meth:`POST
        ~path:Path.Format.("ntp" @/ empty)
        ~query:Query.[ ("flag", (module Single (Bool))) ]
        (Pc_control.Timedate_api.set_ntp t);
      node ~doc:"Set time" ~restrict:[ `Guest; `Operator ] ~meth:`POST
        ~path:Path.Format.("time" @/ empty)
        ~query:Query.[ ("value", (module Single (Time_uri.Show))) ]
        (Pc_control.Timedate_api.set_time t);
    ]

let time_ws (timedate : Pc_control.Timedate.t) =
  let open Api_websocket in
  make ~prefix:"timedate"
    [
      event_node ~doc:"Timedate configuration"
        ~path:Path.Format.("config" @/ empty)
        ~query:Query.empty
        (Pc_control.Timedate_api.Event.get_config timedate);
    ]

let time_pages : 'a. unit -> 'a Api_template.item list =
 fun () ->
  let open Api_template in
  let props =
    make_template_props ~title:"Дата и время"
      ~post_scripts:[ `Src "/js/page-timedate-settings.js" ]
      ~stylesheets:[ "/css/page-timedate-settings.min.css" ]
      ()
  in
  simple ~restrict:[ `Operator; `Guest ] ~priority:(`Index 10)
    ~title:"Дата и время"
    ~icon:(icon Components_tyxml.Svg_icons.timer)
    ~path:(Path.of_string "settings/timedate")
    props
