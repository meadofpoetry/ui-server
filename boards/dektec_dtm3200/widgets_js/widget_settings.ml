open Js_of_ocaml
open Application_types
open Board_dektec_dtm3200_types
open Components

let name = "Настройки"
let settings = None

type event =
  [ `State of Topology.state
  | `Nw_mode of nw
  | `Ip_receive_mode of ip_receive
  ]

let make (state : Topology.state )
    (nw : nw)
    (ip_receive : ip_receive)
    (control : int) =
  let nw = Widget_network_settings.make state nw control in
  let ip = Widget_receiver_settings.make state ip_receive control in
  let tabs =
    [ (nw#widget, Tab.make ~label:"Сеть" ())
    ; (ip#widget, Tab.make ~label:"Приём TSoIP" ()) ] in
  let tab_bar, body = Ui_templates.Tabs.create_simple tabs in
  object
    inherit Widget.t Dom_html.(createDiv document) () as super

    method! init () : unit =
      super#init ();
      super#append_child tab_bar;
      super#append_child body

    method! destroy () : unit =
      super#destroy ();
      tab_bar#destroy ();
      body#destroy ()

    method notify : event -> unit = function
      | `State _ as e -> nw#notify e; ip#notify e
      | `Nw_mode _ as e -> nw#notify e
      | `Ip_receive_mode _ as e -> ip#notify e
  end
