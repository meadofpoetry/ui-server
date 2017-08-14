open Authorize
   
let home_page ?headers =
  match headers with
  | Some headers -> (fun () -> Cohttp_lwt_unix.Server.respond_redirect ~headers ~uri:(Uri.with_path Uri.empty "/") ())
  | None         -> (fun () -> Cohttp_lwt_unix.Server.respond_redirect ~uri:(Uri.with_path Uri.empty "/") ())
                  
let login_page = Cohttp_lwt_unix.Server.respond_redirect ~uri:(Uri.with_path Uri.empty "/login")

let not_found = Cohttp_lwt_unix.Server.respond_not_found

let error_page = function
  | Failure e -> Cohttp_lwt_unix.Server.respond_error ~status:`Unauthorized ~body:e ()
  | _         -> Cohttp_lwt_unix.Server.respond_error ~status:`Unauthorized ~body:"Unknown error" ()
               
let redirect_auth dbs headers request =
  let open Lwt.Infix in
  Lwt.catch 
    (fun () -> auth dbs headers)
    (fun _  -> Lwt.return Need_auth)
  >>= function
    | Id id     -> Lwt.catch (fun () -> request id) (fun e -> error_page e)
    | Need_auth -> Cohttp_lwt_unix.Server.respond_need_auth ~auth:(`Basic "User Visible Realm") ()
    | Done hd   -> home_page ~headers:hd ()

let redirect_if_not id usr request =
  if (User.eq usr id)
  then (request ())
  else Cohttp_lwt_unix.Server.respond_error ~status:`Forbidden ~body:"Operation not permitted." ()

let redirect_if id usr request =
  if not (User.eq usr id)
  then (request ())
  else Cohttp_lwt_unix.Server.respond_error ~status:`Forbidden ~body:"Operation not permitted." ()
