open Containers
open Redirect

type socket_data = Cohttp_lwt_unix.Request.t * Conduit_lwt_unix.flow

let wrap api_call meth args sock_data headers body =
  fun id -> api_call id meth args sock_data headers body

module type HANDLER = sig
  val domain : string
  val handle : User.t -> Cohttp.Code.meth -> string list -> socket_data ->
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
                
let handle tbl redir meth path sock_data headers body =
  match path with
  | key::tl -> let (module H : HANDLER) = Handlers.find tbl key in
               redir @@ wrap H.handle meth tl sock_data headers body
  | _ -> not_found ()

let add_layer (domain : string) (l : (module HANDLER) list) : (module HANDLER) =
  let tbl = create l in
  (module struct
    let domain = domain
    let handle id meth path sock_data headers body =
      match path with
      | key::tl -> let (module H : HANDLER) = Handlers.find tbl key in
                   H.handle id meth tl sock_data headers body
      | _ -> not_found ()
  end : HANDLER)
