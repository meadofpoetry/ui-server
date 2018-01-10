open Api.Interaction   
open Lwt.Infix

let set_password users body () =
  let open User in
  yojson_of_body body >>= fun js ->
  match pass_of_yojson js with
  | Error e -> respond_error e ()
  | Ok pass -> (try set_pass users pass;
                    respond_ok ()
                with _ -> respond_error "pass db err" ())
  
let user_handle users id meth args _ headers body = (*headers body =*)
  let open Api.Redirect in
  let open User in
  let not_root = not @@ User.eq id `Root in
  match meth, args with
  | `POST,   ["password"] -> redirect_if not_root @@ set_password users body
  | `GET,    ["logout"]   -> respond_need_auth ~headers:headers ~auth:(`Basic "User Visible Realm") ()
  | _ -> not_found ()

module Api_handler = Api.Handler.Make(Common.User)
  
let handlers users =
  [ (module struct
       let domain = "user"
       let handle = user_handle users
     end : Api_handler.HANDLER); ]
