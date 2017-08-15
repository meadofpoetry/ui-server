open Api_handler

open Lwt.Infix
   
let get_user _ _ () =
  Cohttp_lwt_unix.Server.respond_string ~status:`OK ~body:"user" ()
   
let user_handle dbs id meth args _ _ = (*headers body =*)
  let open Redirect in
  let open User in
  match meth, args with
  | `GET, [user] -> redirect_if_not id `Root @@ get_user dbs user
  | _ -> not_found ()
  
let handlers dbs =
  [ (module struct
       let domain = "user"
       let handle = user_handle dbs
     end : HANDLER); ]
