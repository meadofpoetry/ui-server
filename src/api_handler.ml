open Containers
open Lwt
open Lwt_react
open Cohttp_lwt_unix
open User
open Containers
open Redirect

let handle ~database
           meth path headers body =
  let redir = redirect_auth database headers in
  match meth, path with
  | `GET, "test"::tl   -> redir (fun _ -> Cohttp_lwt_unix.Server.respond_string ~status:`OK ~body:(String.concat " " tl) ())
  | _ -> not_found ()
