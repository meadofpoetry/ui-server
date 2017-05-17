open Lwt
open Cohttp
open Cohttp_lwt_unix
open Containers

let user = "user"
let pswd = "pswd"
   
let auth_needed headers =
  let open Option in
  Cohttp.Header.get_authorization headers
  |> function
  | None  -> true
  | Some (`Basic (name, pass)) -> false (*name = user && pass = pswd*)
  | _ -> true

let auth headers name pass =
  (* if name = user && pass = pswd then*)
  Server.respond_redirect
         ~headers:(Cohttp.Header.add_authorization headers (`Basic (name, pass)))
         ~uri:(Uri.with_path Uri.empty "/")
         ()
 (* else Server.respond_redirect ~uri:(Uri.with_path Uri.empty "/login") ()
  *)
