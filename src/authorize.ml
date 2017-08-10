open Containers
open Cohttp

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
      Database.select_one dbs
                          [%sql "SELECT @d{type}, @s{password} FROM users \
                                 WHERE login = %s"] name
      >>= fun (t, p) ->
      if pass = p
      then Lwt.return (Some (User.of_int t))
      else Lwt.return None )
         
let auth dbs headers =
  let open Lwt.Infix in
  validate_headers dbs headers >>=
  function
  | None    -> Lwt.return Need_auth
  | Some id -> Lwt.return (Id id)
