open Js_of_ocaml
open Components
open Pc_control_types.Network_config

let ( >>= ) = Lwt.bind

let ( >>=? ) x f = Lwt_result.(map_err Api_js.Http.error_to_string @@ x >>= f)

let make_dialog set =
  let title =
    Js_of_ocaml_tyxml.Tyxml_js.To_dom.of_element
    @@ Dialog.Markup.create_title_simple ~title:"Внимание" () in
  let content =
    Js_of_ocaml_tyxml.Tyxml_js.To_dom.of_element
    @@ Dialog.Markup.create_content_simple
      "Применение настроек может привести к разрыву соединения. \
       Хотите продолжить?"
      () in
  let actions =
    Dialog.[ make_action ~action:Close ~label:"Отмена" ()
           ; make_action ~appearance:Raised ~action:Accept ~label:"Продолжить" ()
           ] in
  let dialog = Dialog.make ~title ~content ~actions () in
  dialog,
  (fun (btn : Button.t) value ->
     dialog#open_await ()
     >>= function
     | Close | Destroy | Custom _ -> Lwt.return_unit
     | Accept ->
       let t = set value in
       btn#set_loading_lwt t;
       t)

let () =
  let scaffold = Scaffold.attach (Dom_html.getElementById "root") in
  let thread =
    Pc_control_http_js.get_config ()
    >>=? fun config ->
    let ethernet = Ethernet_config.make config.ethernet in
    let ipv4 = Ipv4_config.make config.ipv4 in
    let dns = Dns_config.make config.ipv4 in
    let routes = Routes_config.make config.ipv4 in
    let set value =
      Pc_control_http_js.set_config value
      >>= function
      | Ok () ->
        Pc_control_http_js.get_config ()
        >>=? (fun (conf : Pc_control_types.Network_config.t) ->
            ethernet#set_value conf.ethernet;
            ipv4#set_value conf.ipv4;
            dns#set_value conf.ipv4.dns;
            routes#set_value conf.ipv4.routes.static;
            Lwt.return_ok ())
        >>= fun _ -> Lwt.return_unit
      | Error _ -> Lwt.return_unit in
    let confirmation_dialog, open_dialog = make_dialog set in
    let submit = Button.make
        ~appearance:Raised
        ~on_click:(fun btn _ _ ->
            let static = routes#value in
            let value = match ethernet#value, ipv4#value with
              | Some ethernet, Some ipv4 ->
                Some { ethernet
                     ; ipv4 = { ipv4 with dns = dns#value
                                        ; routes = { ipv4.routes with static }
                              }
                     ; connection = config.connection
                     ; ipv6 = ()
                     ; proxy = ()
                     }
              | _ -> None in
            match value with
            | None -> Lwt.return_unit
            | Some x -> open_dialog btn x)
        ~label:"Применить"
        () in
    let form = Widget.create Dom_html.(createDiv document) in
    form#append_child ethernet;
    form#append_child ipv4;
    form#append_child dns;
    form#append_child routes;
    form#append_child submit;
    Dom.appendChild Dom_html.document##.body confirmation_dialog#root;
    form#add_class Page_network_settings_tyxml.CSS.root;
    Lwt.return_ok form in
  let loader = Ui_templates.Loader.create_widget_loader thread in
  scaffold#set_body loader

