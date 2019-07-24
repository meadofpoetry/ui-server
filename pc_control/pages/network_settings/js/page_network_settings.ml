open Js_of_ocaml
open Components

let ( >>= ) x f = Lwt_result.(map_err Api_js.Http.error_to_string @@ x >>= f)

let () =
  let scaffold = Scaffold.attach (Dom_html.getElementById "root") in
  let thread =
    Lwt_result.Infix.(
      Old.page `Root
      >>= fun w -> w#add_class "network-settings"; Lwt.return_ok w) in
  let body = Ui_templates.Loader.create_widget_loader thread in
  scaffold#set_body body

