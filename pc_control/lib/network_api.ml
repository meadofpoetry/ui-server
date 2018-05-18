open Containers
open Api.Interaction
open Api.Redirect
open Lwt.Infix

let set_config network body () =
  let open Json in
  Json.of_body body >>= fun js ->
  Network_config.of_yojson js
  |> function
    | Error _ -> Json.respond_result_unit (Error (`String "bad data"))
    | Ok conf ->
       network#update_settings conf >>= fun () ->
       respond_result_unit (Ok ())

let get_config network () =
  let open Json in
  network#get_settings () >>= function
  | None   -> Json.respond_result (Error (`String "no interface available"))
  | Some r -> Json.respond_result (Ok (Network_config.to_yojson r))

let network_handler network id meth args _ headers body =
  let not_root = not @@ Common.User.eq id `Root in
  match meth, args with
  | `POST,   ["config"] -> redirect_if not_root @@ set_config network body
  | `GET,    ["config"] -> get_config network ()
  | _ -> not_found ()

module Api_handler = Api.Handler.Make(Common.User)
                   
let handlers network =
  [ (module struct
       let domain = "network"
       let handle = network_handler network
     end : Api_handler.HANDLER); ]
