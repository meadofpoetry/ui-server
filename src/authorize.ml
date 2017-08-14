open Containers
open Cohttp
open User
   
type auth_result = Id of User.user
                 | Done of Header.t
                 | Need_auth
                 
let validate_headers dbs hds =
  let open Lwt.Infix in
  Header.get_authorization hds
  |> function
  | None -> Lwt.return None
  | Some x -> match x with
    | `Other _ -> Lwt.return None
    | `Basic (name, pass) -> (
      Storage.request dbs (Get_pass name)
      >>= fun u ->
      if pass = u.password
      then Lwt.return (Some u.user)
      else Lwt.return None )
         
let auth dbs headers =
  let open Lwt.Infix in
  validate_headers dbs headers >>=
  function
  | None    -> Lwt.return Need_auth
  | Some id -> Lwt.return (Id id)
