open Cohttp
   
type error = [`Need_auth | `Wrong_password]

(* TODO there are more possible errors *)
           
let auth validate headers =
  Header.get_authorization headers
  |> function
    | None ->
       Lwt.return_error `Need_auth
    | Some x ->
       match x with
       | `Other _ ->
          Lwt.return_error `Need_auth
       | `Basic (name, pass) ->
          validate ~name ~pass
