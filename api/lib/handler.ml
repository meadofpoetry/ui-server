open Containers
open Redirect
   
module Make ( User : sig type t end ) = struct
  
  type socket_data = Cohttp_lwt_unix.Request.t * Conduit_lwt_unix.flow

  let wrap api_call meth args sock_data headers body =
    fun id -> api_call id meth args sock_data headers body

  module type HANDLER = sig
    val domain : string
    val handle : User.t -> Cohttp.Code.meth -> Common.Uri.sep -> socket_data ->
                 Cohttp.Header.t -> Cohttp_lwt.Body.t -> (Cohttp.Response.t * Cohttp_lwt.Body.t) Lwt.t
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
    
  let handle tbl redir meth uri_sep sock_data headers body =
    match Common.Uri.sep_path uri_sep with
    | key::tl -> let (module H : HANDLER) = Handlers.find tbl key in
                 redir @@ wrap H.handle meth (Common.Uri.upgrade_path uri_sep tl) sock_data headers body
    | _ -> not_found ()

  let add_layer (domain : string) (l : (module HANDLER) list) : (module HANDLER) =
    let tbl = create l in
    (module struct
       let domain = domain
       let handle id meth uri_sep sock_data headers body =
         match Common.Uri.sep_path uri_sep with
         | key::tl -> let (module H : HANDLER) = Handlers.find tbl key in
                      H.handle id meth (Common.Uri.upgrade_path uri_sep tl) sock_data headers body
         | _ -> not_found ()
     end : HANDLER)

end
