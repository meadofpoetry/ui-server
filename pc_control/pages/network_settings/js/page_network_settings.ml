open Js_of_ocaml
open Js_of_ocaml_tyxml
open Components
open Pc_control_types.Network_config
module D = Page_network_settings_tyxml.Make (Tyxml_js.Xml) (Tyxml_js.Svg) (Tyxml_js.Html)

let ( >>= ) = Lwt.bind

let ( >>=? ) = Lwt_result.bind

let make_dialog set =
  let open Dialog.D in
  let title = dialog_title ~title:"Внимание" () in
  let content =
    dialog_content
      ~children:
        [ Tyxml_js.Html.txt
            "Применение настроек может привести к \
             разрыву соединения. Хотите продолжить?" ]
      ()
  in
  let actions =
    [ dialog_action ~action:Close ~label:"Отмена" ()
    ; dialog_action ~appearance:Raised ~action:Accept ~label:"Продолжить" () ]
  in
  let dialog = Dialog.make ~title ~content ~actions () in
  ( dialog
  , fun (btn : Button.t) value ->
      dialog#open_await ()
      >>= function
      | Close | Destroy | Custom _ -> Lwt.return_unit
      | Accept ->
          let t = set value in
          btn#set_loading_lwt t;
          t )

let make_submit_button ethernet ipv4 dns routes config open_dialog =
  Button.make
    ~appearance:Raised
    ~on_click:(fun btn _ _ ->
      let static = routes#value in
      let value =
        match ethernet#value, ipv4#value with
        | Some ethernet, Some ipv4 ->
            Some
              { ethernet
              ; ipv4 = {ipv4 with dns = dns#value; routes = {ipv4.routes with static}}
              ; connection = config.connection
              ; ipv6 = ()
              ; proxy = () }
        | _ -> None
      in
      match value with
      | None -> Lwt.return_unit
      | Some x -> open_dialog btn x)
    ~label:"Применить"
    ()

let () =
  let (scaffold : Scaffold.t) = Js.Unsafe.global##.scaffold in
  let snackbar = Snackbar.make ~label:(`Text "") () in
  let thread =
    Pc_control_http_js.Network.get_config ()
    >>=? fun config ->
    Api_js.Websocket.JSON.open_socket ~path:(Netlib.Uri.Path.Format.of_string "ws") ()
    >>=? fun socket ->
    Pc_control_http_js.Network.Event.get_config socket
    >>=? fun (event_id, event) ->
    let ethernet = Ethernet_config.make config.ethernet in
    let ipv4 = Ipv4_config.make config.ipv4 in
    let dns = Dns_config.make config.ipv4 in
    let routes = Routes_config.make config.ipv4 in
    let update_config (conf : Pc_control_types.Network_config.t) =
      ethernet#set_value conf.ethernet;
      ipv4#set_value conf.ipv4;
      dns#set_value conf.ipv4.dns;
      routes#set_value conf.ipv4.routes.static
    in
    let set value =
      let ( >>=? ) = Lwt_result.bind in
      let thread =
        Pc_control_http_js.Network.set_config value
        >>=? fun () ->
        Pc_control_http_js.Network.get_config ()
        >>=? fun (conf : Pc_control_types.Network_config.t) ->
        update_config conf;
        Lwt.return_ok ()
      in
      thread
      >>= function
      | Ok () -> Lwt.return_unit
      | Error (`Msg e) ->
          snackbar#set_label_text e;
          scaffold#show_snackbar snackbar
    in
    let confirmation_dialog, open_dialog = make_dialog set in
    let submit = make_submit_button ethernet ipv4 dns routes config open_dialog in
    Dom.appendChild Dom_html.document##.body confirmation_dialog#root;
    let page =
      Widget.create
      @@ Js_of_ocaml_tyxml.Tyxml_js.To_dom.of_element
      @@ D.create
           ~children:
             [ethernet#markup; ipv4#markup; dns#markup; routes#markup; submit#markup]
           ()
    in
    let event' = React.E.map update_config event in
    page#set_on_destroy (fun () ->
        Element.remove_child_safe Dom_html.document##.body confirmation_dialog#root;
        snackbar#destroy ();
        confirmation_dialog#destroy ();
        React.E.stop ~strong:true event';
        React.E.stop ~strong:true event;
        Lwt.async (fun () ->
            Api_js.Websocket.JSON.unsubscribe socket event_id
            >>= function
            | Error e -> Lwt.fail (Api_js.Websocket.Error e)
            | Ok v -> Lwt.return v);
        Api_js.Websocket.close_socket socket);
    Lwt.return_ok page
  in
  let (_ : Dom_html.element Js.t) =
    Components_lab.Loader.make_widget_loader ~elt:scaffold#app_content_inner thread
  in
  ()
