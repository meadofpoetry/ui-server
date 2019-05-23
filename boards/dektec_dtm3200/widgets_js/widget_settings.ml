open Application_types
open Board_dektec_dtm3200_types
open Components

let name = "Настройки"
let settings = None

let make ~(state : Topology.state React.signal)
    ~(nw : nw React.signal)
    ~(ip : ip_receive React.signal)
    control =
  let nw = Widget_network_settings.make ~state ~mode:nw control in
  let ip = Widget_receiver_settings.make ~state ~mode:ip control in
  let tabs =
    [ new Tab.t ~content:(Text "Сеть") ~value:nw ()
    ; new Tab.t ~content:(Text "Приём TSoIP") ~value:ip () ] in
  Ui_templates.Tabs.(create_simple tabs |> fun (a, b) -> wrap a b)
