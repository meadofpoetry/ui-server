open Authorize
   
let home_page ?headers =
  match headers with
  | Some headers -> (fun () -> Cohttp_lwt_unix.Server.respond_redirect ~headers ~uri:(Uri.with_path Uri.empty "/") ())
  | None         -> (fun () -> Cohttp_lwt_unix.Server.respond_redirect ~uri:(Uri.with_path Uri.empty "/") ())
                  
let login_page = Cohttp_lwt_unix.Server.respond_redirect ~uri:(Uri.with_path Uri.empty "/login")

let not_found = Cohttp_lwt_unix.Server.respond_not_found

let error_page e = Cohttp_lwt_unix.Server.respond_error ~status:`Unauthorized ~body:e ()
          
let redirect_auth dbs headers request =
  let open Lwt.Infix in
  auth dbs headers
  >>= function
    | Id id     -> request id
    | Need_auth -> Cohttp_lwt_unix.Server.respond_need_auth ~auth:(`Basic "User Visible Realm") ()
    | Done hd   -> home_page ~headers:hd ()
