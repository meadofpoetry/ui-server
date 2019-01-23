open Js_of_ocaml
open Lwt.Infix
open Components

let () =
  let user = Js_of_ocaml.(Js.to_string @@ Js.Unsafe.variable "username") in
  let user = match Common.User.of_string user with
    | Error e -> failwith e
    | Ok user -> user in
  let loader =
    Ui_templates.Loader.create_widget_loader
      (Pc_control_js.Network.page user >>= Lwt.return_ok) in
  let scaffold = Scaffold.attach (Dom_html.getElementById "root") in
  scaffold#set_body loader
