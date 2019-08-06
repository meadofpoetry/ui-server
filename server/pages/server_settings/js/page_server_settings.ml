open Js_of_ocaml
open Js_of_ocaml_tyxml
open Components
open Application_types
open Netlib.Uri

let ( >>= ) = Lwt.bind

let ( >>=? ) x f = Lwt_result.(map_err Api_js.Http.error_to_string @@ x >>= f)

module Api_http = Api_js.Http.Make(Body)

let get_user () =
  let user = Js.to_string @@ Js.Unsafe.global##.username in
  match Application_types.User.of_string user with
  | Error e -> failwith e
  | Ok user -> user

let () =
  let (scaffold : Scaffold.t) = Js.Unsafe.global##.scaffold in
  let thread =
    Server_http_js.get_config ()
    >>=? fun config ->
    Api_js.Websocket.JSON.open_socket ~path:(Path.Format.of_string "ws") ()
    >>=? fun socket -> Server_http_js.Event.get_config socket
    >>=? fun (event_id, event) ->
    let disclaimer =
      Tyxml_js.Html.(
        div ~a:[a_class ["disclaimer"]]
          [txt "Внимание! Настройки безопасности вступают в силу \
                только после перезагрузки прибора."]) in
    let https = Https_config.make
        ~set_snackbar:scaffold#show_snackbar
        config in
    let certificate = Certificate_config.make
        ~set_snackbar:scaffold#show_snackbar
        config in
    let page =
      Widget.create
      @@ Tyxml_js.To_dom.of_element
      @@ Markup.make [ disclaimer
                     ; https#markup
                     ; certificate#markup ] in
    let event' = React.E.map (fun (config : Server_types.settings) ->
        https#set_value config.https_enabled;
        certificate#set_value config) event in
    page#set_on_destroy (fun () ->
        React.E.stop ~strong:true event';
        React.E.stop ~strong:true event;
        Lwt.async (fun () -> Api_js.Websocket.JSON.unsubscribe socket event_id);
        Api_js.Websocket.close_socket socket);
    Lwt.return_ok page in
  let body = Ui_templates.Loader.create_widget_loader thread in
  scaffold#set_body body
