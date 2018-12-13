open Lwt.Infix

let () =
  let user = Js_of_ocaml.(Js.to_string @@ Js.Unsafe.variable "username") in
  let user = match Common.User.of_string user with
    | Error e -> failwith e
    | Ok user -> user
  in
  Lwt.async (fun () ->
      Pc_control_js.Network.page user >>= fun page ->
      ignore @@ new Ui_templates.Page.t (`Static [page]) ();
      Lwt.return_unit)
