open Api_handler
open Interaction

open Lwt.Infix

let set_password dbs body () =
  yojson_of_body body >>= fun js ->
  match User.pass_of_yojson js with
  | Error e -> respond_error e ()
  | Ok pass -> User.Storage.(request dbs (Set_passwd pass))
               >>= fun () -> respond_ok ()
  
let user_handle dbs id meth args _ body = (*headers body =*)
  let open Redirect in
  let open User in
  let not_root = not @@ User.eq id `Root in
  match meth, args with
  | `POST,   ["pass"]      -> redirect_if not_root @@ set_password dbs body
  | _ -> not_found ()
  
let handlers dbs =
  [ (module struct
       let domain = "user"
       let handle = user_handle dbs
     end : HANDLER); ]
