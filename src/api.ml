open Lwt
open Lwt_react
open Cohttp_lwt_unix
open User

let handle meth path headers body =
  match meth, path with
  | `GET, ["auth"]  -> User.auth headers "user" "pswd"
  | `GET, ["test"]  -> Cohttp_lwt_unix.Server.respond_string ~status:`OK ~body:"test" ()
  | _ -> Cohttp_lwt_unix.Server.respond_not_found ()
