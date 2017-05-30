open Redirect

let wrap api_call meth args headers body =
  fun id -> api_call id meth args headers body
          
let handle ~database
           meth path headers body =
  let redir = redirect_auth database headers in
  match meth, path with
  | `GET, "test"::tl   -> redir @@ wrap Api.Test.test meth tl headers body
  | _ -> not_found ()
