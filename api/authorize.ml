open Containers
open Cohttp
   
type 'a auth_result = Id of 'a
                    | Done of Header.t
                    | Need_auth
                 
let validate_headers validate hds =
  let open Lwt.Infix in
  Header.get_authorization hds
  |> function
  | None -> Lwt.return None
  | Some x -> match x with
    | `Other _ -> Lwt.return None
    | `Basic (name, pass) -> (
      match validate name pass with
      | Error _ -> Lwt.return_none
      | Ok user -> Lwt.return_some user
    )
         
let auth validate headers =
  let open Lwt.Infix in
  validate_headers validate headers >>=
  function
  | None    -> Lwt.return Need_auth
  | Some id -> Lwt.return (Id id)
