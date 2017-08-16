open Containers
open Redirect

let wrap api_call meth args headers body =
  fun id -> api_call id meth args headers body

module type HANDLER = sig
  val domain : string
  val handle : User.t -> Cohttp.Code.meth -> string list ->
               Cohttp.Header.t -> Cohttp_lwt_body.t -> (Cohttp.Response.t * Cohttp_lwt_body.t) Lwt.t
end

module Handlers = Hashtbl.Make(String)

let create hndls =
  let tbl = Handlers.create 50 in
  List.iter (fun ((module H : HANDLER) as handler) ->
      try  ignore @@ Handlers.find tbl H.domain;
           failwith ("Domain " ^ H.domain ^ " already exists")
      with Not_found -> Handlers.add tbl H.domain handler)
            hndls;
  tbl
                
let handle tbl redir meth path headers body =
  match path with
  | key::tl -> let (module H : HANDLER) = Handlers.find tbl key in
               redir @@ wrap H.handle meth tl headers body
  | _ -> not_found ()
