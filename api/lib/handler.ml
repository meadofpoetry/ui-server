open Containers
open Redirect
   
module Make ( User : sig type t end ) = struct
  
  type socket_data = Cohttp_lwt_unix.Request.t * Conduit_lwt_unix.flow

  type call = (User.t -> Cohttp.Code.meth ->
               Cohttp.Header.t -> Cohttp_lwt.Body.t -> socket_data ->
               (Cohttp.Response.t * Cohttp_lwt.Body.t) Lwt.t) Common.Uri.Dispatcher.node
          
  let wrap api_call uri meth headers body sock_data =
    fun id -> api_call uri id meth headers body sock_data
            
  module type HANDLER = sig
    val domain : string
    val handle : Common.Uri.uri -> User.t -> Cohttp.Code.meth ->
                 Cohttp.Header.t -> Cohttp_lwt.Body.t -> socket_data ->
                 (Cohttp.Response.t * Cohttp_lwt.Body.t) Lwt.t
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
    
  let handle tbl redir uri meth headers body sock_data =
    let root, path = Common.Uri.(Path.next uri.path) in
    match root with
    | Some key -> let (module H : HANDLER) = Handlers.find tbl key in
                  redir @@ wrap H.handle (Common.Uri.upgrade_path uri path) meth headers body sock_data
    | _ -> not_found ()

  let add_layer (domain : string) (l : (module HANDLER) list) : (module HANDLER) =
    let tbl = create l in
    (module struct
       let domain = domain
       let handle uri id meth headers body sock_data =
         let root, path = Common.Uri.(Path.next uri.path) in
         match root with
         | Some key -> let (module H : HANDLER) = Handlers.find tbl key in
                       H.handle (Common.Uri.upgrade_path uri path) id meth headers body sock_data
         | _ -> (Logs.debug (fun m -> m "(Api_handler) failure in %s %s" domain Common.Uri.(to_string @@ of_uri uri));
                 not_found ()) (* TODO error *)
     end : HANDLER)

  let create_dispatcher (domain : string) (calls : call list) : (module HANDLER) =
    let open Common.Uri in
    let tbl = List.fold_left (fun acc node -> Dispatcher.add acc node) Dispatcher.empty calls in
    (module struct
       let domain = domain
       let handle uri id meth headers body sock_data =
         try Dispatcher.dispatch tbl uri id meth headers body sock_data
         with _ -> (Logs.err (fun m -> m "(Api_handler) failure in %s %s" domain Common.Uri.(to_string @@ of_uri uri));
                    not_found ()) (* TODO error *)
     end : HANDLER)
    
end
