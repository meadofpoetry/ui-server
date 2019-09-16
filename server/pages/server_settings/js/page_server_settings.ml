open Js_of_ocaml
open Components
open Application_types
open Netlib.Uri
module Markup_js =
  Page_server_settings_tyxml.Make
    (Js_of_ocaml_tyxml.Tyxml_js.Xml)
    (Js_of_ocaml_tyxml.Tyxml_js.Svg)
    (Js_of_ocaml_tyxml.Tyxml_js.Html)

let ( >>= ) = Lwt.bind

let ( >>=? ) = Lwt_result.bind

module Api_http = Api_js.Http.Make (Body)

let () =
  let (scaffold : Scaffold.t) = Js.Unsafe.global##.scaffold in
  let thread =
    Server_http_js.get_config ()
    >>=? fun config ->
    Api_js.Websocket.JSON.open_socket ~path:(Path.Format.of_string "ws") ()
    >>=? fun socket ->
    Server_http_js.Event.get_config socket
    >>=? fun (event_id, event) ->
    let disclaimer =
      Js_of_ocaml_tyxml.Tyxml_js.Html.(
        div
          ~a:[a_class ["disclaimer"]]
          [ txt
              "Внимание! Настройки безопасности \
               вступают в силу только после \
               перезагрузки прибора." ])
    in
    let (https : Https_config.t) =
      Https_config.make ~set_snackbar:scaffold#show_snackbar config
    in
    let certificate =
      Certificate_config.make ~set_snackbar:scaffold#show_snackbar config
    in
    let page =
      Widget.create
      @@ Js_of_ocaml_tyxml.Tyxml_js.To_dom.of_element
      @@ Markup_js.create ~children:[disclaimer; https#markup; certificate#markup] ()
    in
    let event' =
      React.E.map
        (fun (config : Server_types.settings) ->
          https#set_value config.https_enabled;
          certificate#set_value config)
        event
    in
    page#set_on_destroy (fun () ->
        React.E.stop ~strong:true event';
        React.E.stop ~strong:true event;
        Lwt.async (fun () -> Api_js.Websocket.JSON.unsubscribe socket event_id);
        Api_js.Websocket.close_socket socket);
    Lwt.return_ok page
  in
  let (_ : Dom_html.element Js.t) =
    Components_lab.Loader.make_widget_loader ~elt:scaffold#app_content_inner thread
  in
  ()
