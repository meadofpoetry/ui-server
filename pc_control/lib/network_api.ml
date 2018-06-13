open Containers
open Api.Interaction
open Api.Redirect
open Lwt.Infix

let set_config (network : Network.t) body () =
  let open Json in
  Json.of_body body >>= fun js ->
  Network_config.of_yojson js
  |> function
    | Error _ -> respond_result_unit (Error (`String "bad data"))
    | Ok conf ->
       Network.apply_ext_settings network conf
       >>= function
       | Ok _ as ok -> respond_result_unit ok
       | Error e    -> respond_result_unit (Error (`String e))

let get_config (network : Network.t) () =
  let open Json in
  Network.get_ext_settings network >>= function
  | Error e -> Json.respond_result (Error (`String e))
  | Ok    r -> Json.respond_result (Ok (Network_config.to_yojson r))

let network_handler (network : Network.t) id meth uri_sep _ headers body =
  let not_root = not @@ Common.User.eq id `Root in
  match meth, Common.Uri.sep_path uri_sep with
  | `POST,   ["config"] -> redirect_if not_root @@ set_config network body
  | `GET,    ["config"] -> get_config network ()
  | _ -> not_found ()

module Api_handler = Api.Handler.Make(Common.User)
                   
let handlers (network : Network.t) =
  [ (module struct
       let domain = "network"
       let handle = network_handler network
     end : Api_handler.HANDLER); ]
