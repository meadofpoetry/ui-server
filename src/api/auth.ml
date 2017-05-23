open Containers
open Lwt
open Cohttp_lwt_unix
open Api_handler
open Database
open User
open Redirect

type state = Database.database

let queries = [ `POST, "login"
              ; `POST, "logout"
              ]

let handle st (meth, route) args header body =
  let redir = redirect_auth st header in
  match (meth, route, args) with
  | `POST, "login", _  -> redir (fun _ -> home_page ()) (fun () -> Authorize.auth st header)
  | `POST, "logout", [] -> Cohttp_lwt_unix.Server.respond_string ~status:`OK ~body:"test" ()
  | _ -> not_found ()
