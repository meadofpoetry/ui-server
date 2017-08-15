open Api_handler
open Interaction

open Lwt.Infix

let js_resp conv data =
  (conv data)
  |> fun js -> respond_js js ()
             
let add_user dbs body () =
  yojson_of_body body >>= fun js ->
  match User.full_of_yojson js with
  | Error e -> respond_error e ()
  | Ok data -> User.Storage.(request dbs (Add_user data))
               >>= fun _ -> respond_ok ()

let rm_user dbs name () =
  User.Storage.(request dbs (Rm_user name))
  >>= fun () -> respond_ok ()

let set_typ dbs body () =
  yojson_of_body body >>= fun js ->
  match User.user_of_yojson js with
  | Error e -> respond_error e ()
  | Ok user -> User.Storage.(request dbs (Set_typ user))
               >>= fun () -> respond_ok ()

let get_user dbs name () =
  User.Storage.(request dbs (Get_user name))
  >>= js_resp User.info_to_yojson

let get_users dbs () =
  User.Storage.(request dbs Get_users)
  >>= js_resp User.info_list_to_yojson

let get_password dbs name () =
  User.Storage.(request dbs (Get_pass name))
  >>= js_resp User.pass_to_yojson

let set_password dbs not_self body () =
  yojson_of_body body >>= fun js ->
  match User.pass_of_yojson js with
  | Error e -> respond_error e ()
  | Ok pass ->
     if (not_self pass.user.name)
     then respond_error "wrong user" ()
     else User.Storage.(request dbs (Set_passwd pass))
          >>= fun () -> respond_ok ()

let set_info dbs not_self body () =
  yojson_of_body body >>= fun js ->
  match User.info_of_yojson js with
  | Error e -> respond_error e ()
  | Ok info ->
     if (not_self info.user.name)
     then respond_error "wrong user" ()
     else User.Storage.(request dbs (Set_info info))
          >>= fun () -> respond_ok ()
  
let user_handle dbs id meth args _ body = (*headers body =*)
  let open Redirect in
  let open User in
  let not_root = not @@ User.eq id.typ `Root in
  let not_self = ((<>) id.name) in
  match meth, args with
  | `POST,   ["add"]       -> redirect_if not_root @@ add_user dbs body
  | `DELETE, [user]        -> redirect_if not_root @@ rm_user dbs user
  | `POST,   ["dignity"]   -> redirect_if not_root @@ set_typ dbs body
  | `GET,    [user]        -> redirect_if (not_self user) @@ get_user dbs user
  | `GET,    []            -> redirect_if not_root @@ get_users dbs
  | `GET,    ["pass";user] -> redirect_if (not_self user) @@ get_password dbs user
  | `POST,   ["pass"]      -> set_password dbs not_self body ()
  | `POST,   ["info"]      -> set_info dbs not_self body ()
  | _ -> not_found ()
  
let handlers dbs =
  [ (module struct
       let domain = "user"
       let handle = user_handle dbs
     end : HANDLER); ]
