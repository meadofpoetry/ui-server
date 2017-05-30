open Containers
open Cohttp

type auth_result = Id of User.user
                 | Done of Header.t
                 | Need_auth

let validate_headers dbs hds =
  ignore dbs; (* TODO *)
  let open Option in
  Header.get_authorization hds
  >>= function
  | `Other _ -> None
  | `Basic (name, pass) -> 
  if name = "root" && pass = "pswd"
  then Some (User.of_string name)
  else None
         
let auth dbs headers =
  match validate_headers dbs headers with
  | None    -> Need_auth
  | Some id -> Id id
