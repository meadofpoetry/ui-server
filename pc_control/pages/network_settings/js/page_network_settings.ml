open Js_of_ocaml
open Components
open Pc_control_types.Network_config

let ( >>= ) = Lwt.bind

let ( >>=? ) x f = Lwt_result.(map_err Api_js.Http.error_to_string @@ x >>= f)

let () =
  let scaffold = Scaffold.attach (Dom_html.getElementById "root") in
  let thread =
    Pc_control_http_js.get_config ()
    >>=? fun config ->
    let ethernet = Ethernet_config.make config.ethernet in
    let ip = Ipv4_config.make config.ipv4 in
    let dns = Dns_config.make config.ipv4 in
    let routes = Routes_config.make config.ipv4 in
    let submit = Button.make
        ~appearance:Raised
        ~on_click:(fun _ _ _ ->
            let ipv4 =
              { dns = dns#value
              ; address = Ipaddr.V4.any, 24l
              ; routes = { gateway = None; static = [] }
              ; meth = Auto
              } in
            let value =
              { ethernet = { mac_address = Macaddr.broadcast }
              ; ipv4
              ; connection = config.connection
              ; ipv6 = ()
              ; proxy = ()
              } in
            Pc_control_http_js.set_config value
            >>= function
            | Ok () -> Lwt.return_unit
            | Error _ -> Lwt.return_unit)
        ~label:"Применить"
        () in
    let form = Widget.create Dom_html.(createDiv document) in
    form#append_child ethernet;
    form#append_child ip;
    form#append_child dns;
    form#append_child routes;
    form#append_child submit;
    form#add_class Page_network_settings_tyxml.CSS.root;
    form#add_class Layout_grid.CSS.root;
    Lwt.return_ok form in
  let loader = Ui_templates.Loader.create_widget_loader thread in
  scaffold#set_body loader

