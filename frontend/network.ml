open Pc_control_js.Network
open Lwt.Infix
   
let () =
  let user = Js.to_string @@ Js.Unsafe.variable "username" in
  let user = match Common.User.of_string user with
    | Error e -> failwith e
    | Ok user -> user
  in
  Lwt.async (fun () ->
      Pc_control_js.Network.page user >>= fun page ->
      let _ = new Ui_templates.Page.t (`Static [page]) () in
      Lwt.return_unit)
