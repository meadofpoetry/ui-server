open Lwt
open Cohttp
open Cohttp_lwt_unix
open Containers
open User
open Database

type login_entry = { name     : string
                   ; password : string
                   }

type auth_result = Id of User.user
                 | Done of Header.t
                 | Need_auth

let login_entry_of_headers hds =
  match Header.get_authorization hds with
  | Some (`Basic (name,password)) -> Some { name; password }
  | _                             -> None
         
let auth dbs headers =
  match login_entry_of_headers headers with
  | None -> Need_auth
  | Some np ->
     if np.name = "root" && np.password = "pswd"
     then Id Root
     else Need_auth
