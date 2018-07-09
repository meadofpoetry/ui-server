open Containers
open Api.Interaction
open Api.Redirect
open Lwt.Infix

let set_config (network : Network.t) _ body () =
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

let get_config (network : Network.t) _ body () =
  let open Json in
  Network.get_ext_settings network >>= function
  | Error e -> Json.respond_result (Error (`String e))
  | Ok    r -> Json.respond_result (Ok (Network_config.to_yojson r))

module Api_handler = Api.Handler.Make(Common.User)
                   
let handlers (network : Network.t) =
  let open Common.Uri in
  let open Api_handler in
  create_dispatcher
    "network" []
    [ `GET, [ create_handler ~docstring:"Network configuration"
                ~path:Path.Format.("config" @/ empty)
                ~query:Query.empty
                (get_config network) ]
    ; `POST, [ create_handler ~docstring:"Network configuration"
                 ~restrict:[ `Guest; `Operator ]
                 ~path:Path.Format.("config" @/ empty)
                 ~query:Query.empty
                 (set_config network) ]
    ]
