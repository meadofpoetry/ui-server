open Lwt
open Lwt_react
open Cohttp_lwt_unix
open User
open Redirect

let handle ~database
           meth path headers body =
  let redir = redirect_auth database headers in
  match meth, path with
  | `POST, ["login"]  -> redir (fun _ -> home_page ()) (fun () -> Authorize.auth database headers)
  | `GET,  ["test"]   -> Cohttp_lwt_unix.Server.respond_string ~status:`OK ~body:"test" ()
  | _ -> not_found ()
