open Js_of_ocaml
open Js_of_ocaml_tyxml
open Components
open Application_types

let ( >>= ) = Lwt.bind

let ( >>=? ) x f = Lwt_result.(map_err Api_js.Http.error_to_string @@ x >>= f)

module Api_http = Api_js.Http.Make(Body)

let get_user () =
  let user = Js.to_string @@ Js.Unsafe.global##.username in
  match Application_types.User.of_string user with
  | Error e -> failwith e
  | Ok user -> user

let () =
  let scaffold = Scaffold.attach (Dom_html.getElementById "root") in
  let thread =
    Server_http_js.get_config ()
    >>=? fun config ->
    let https = Https_config.make config in
    let certificate = Certificate_config.make
        ~set_snackbar:scaffold#show_snackbar
        config in
    let page =
      Widget.create
      @@ Tyxml_js.To_dom.of_element
      @@ Markup.make [ https#markup
                     ; certificate#markup ] in
    Lwt.return_ok page in
  let body = Ui_templates.Loader.create_widget_loader thread in
  scaffold#set_body body
