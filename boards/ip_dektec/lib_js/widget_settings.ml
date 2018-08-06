open Board_types
open Containers
open Components
open Common

type config = unit [@@deriving yojson]

let name     = "Настройки"
let settings = None

let make ~(state: Topology.state React.signal)
         ~(nw:    nw React.signal)
         ~(ip:    ip React.signal)
         (_:      config option)
         control =
  let nw = Widget_network_settings.make ~state ~mode:nw None control in
  let ip = Widget_receiver_settings.make ~state ~mode:ip None control in
  [ new Tab.t ~content:(Text "Сеть") ~value:nw ()
  ; new Tab.t ~content:(Text "Приём TSoIP") ~value:ip () ]
  |> Ui_templates.Tabs.create_simple_tabs
  |> Widget.coerce
