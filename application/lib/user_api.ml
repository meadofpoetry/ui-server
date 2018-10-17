open Api.Interaction.Json
open Api.Interaction

module Api_handler = Api.Handler.Make(Common.User)

let set_password (users:User.entries) _ body () =
  let open User in
  of_body body >>= fun js ->
  match pass_change_of_yojson js with
  | Error e -> respond_error e ()
  | Ok pass ->
     try if (get_pass users pass.user).pass = pass.old_pass
         then (set_pass users { user = pass.user; password = pass.new_pass };
               respond_result_unit (Ok ()))
         else respond_error "bad pass" ()
     with _ -> respond_error "pass db err" ()

let logout headers body () =
  respond_need_auth ~headers:headers ~auth:(`Basic "User Visible Realm") ()

let handlers users =
  let open Common.Uri in
  let open Api_handler in
  create_dispatcher
    "user"
    [ ]
    [ `POST, [ create_handler ~docstring:"Changes user password"
                 ~restrict:[ `Guest; `Operator ]
                 ~path:Path.Format.("password" @/ empty)
                 ~query:Query.empty
                 (set_password users)
             ]
    ; `GET,  [ create_handler ~docstring:"Log out from current session"
                 ~path:Path.Format.("logout" @/ empty)
                 ~query:Query.empty
                 logout
             ]
    ]
