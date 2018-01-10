open Containers
open Cohttp
   
type 'a auth_result = Id of 'a
                    (*| Done of Header.t*)
                    | Need_auth
                    | Unknown of string
                 
let validate_headers validate hds =
  let open Lwt.Infix in
  Header.get_authorization hds
  |> function
  | None -> Lwt.return_error `Need_auth
  | Some x -> match x with
    | `Other _ -> Lwt.return_error `Need_auth
    | `Basic (name, pass) -> (
      match validate name pass with
      | Error e -> Lwt.return_error e
      | Ok user -> Lwt.return_ok user
    )
         
let auth validate headers =
  let open Lwt.Infix in
  validate_headers validate headers >>=
    function
    | Error (`Need_auth) | Error (`Wrong_password) -> Lwt.return Need_auth
    | Error (`Unknown s)   -> Lwt.return (Unknown s)
    | Ok (id)              -> Lwt.return (Id id)
