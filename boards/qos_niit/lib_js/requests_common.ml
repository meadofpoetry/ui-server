open Common

let req_to_uri ?(uri=Uri.empty) control req : Uri.t =
  let path = ["/api";"board";string_of_int control] @ (Api_utils.req_to_path req)
             |> String.concat "/" in
  Uri.with_path uri path

let ( >>* ) (u,x) f = match x with
  | Some x -> f (u,x)
  | None   -> u,None
