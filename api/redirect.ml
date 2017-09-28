open Authorize
open Interaction
   
let home_page ?headers =
  match headers with
  | Some headers -> (fun () -> respond_redirect ~headers "/" ())
  | None         -> (fun () -> respond_redirect "/" ())
                  
let login_page = respond_redirect "/login"

let not_found = respond_not_found

let error_page ?(status = `Unauthorized) = function
  | Failure e -> respond_error ~status e ()
  | _         -> respond_error ~status "Unknown error" ()
               
let redirect_auth dbs headers request =
  let open Lwt.Infix in
  Lwt.catch 
    (fun () -> auth dbs headers)
    (fun _  -> Lwt.return Need_auth)
  >>= function
    | Id id     -> Lwt.catch (fun () -> request id) (fun e -> error_page e)
    | Need_auth -> respond_need_auth ~auth:(`Basic "User Visible Realm") ()
    | Done hd   -> home_page ~headers:hd ()

let redirect_if p request =
  if not p
  then request ()
  else respond_error "Operation not permitted." ()
