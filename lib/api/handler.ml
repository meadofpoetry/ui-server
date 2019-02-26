open Containers
open Redirect

module Make ( User : sig type t val equal : t -> t -> bool end ) = struct

  open Netlib
  
  type socket_data = Cohttp_lwt_unix.Request.t * Conduit_lwt_unix.flow

  type http_handler = (User.t -> Cohttp.Header.t -> Cohttp_lwt.Body.t ->
                       (Cohttp.Response.t * Cohttp_lwt.Body.t) Lwt.t) Uri.Dispatcher.node

  type ws_handler = (User.t -> Cohttp.Header.t -> Cohttp_lwt.Body.t -> socket_data ->
                     (Cohttp.Response.t * Cohttp_lwt.Body.t) Lwt.t) Uri.Dispatcher.node

  let wrap api_call uri meth headers body sock_data =
    fun id -> api_call uri id meth headers body sock_data

  module type HANDLER = sig
    val domain : string
    val handle : Uri.t -> User.t -> Cohttp.Code.meth ->
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
    let path = Uri.Path.of_uri uri in
    let root, path = Uri.Path.next path in
    match root with
    | Some key -> let (module H : HANDLER) = Handlers.find tbl key in
                  redir @@ wrap H.handle (Uri.with_path_parsed uri path) meth headers body sock_data
    | _ -> not_found ()

  let add_layer (domain : string) (l : (module HANDLER) list) : (module HANDLER) =
    let tbl = create l in
    (module struct
       let domain = domain
       let handle uri id meth headers body sock_data =
         let path = Uri.Path.of_uri uri in
         let root, path = Uri.Path.next path in
         match root with
         | Some key -> let (module H : HANDLER) = Handlers.find tbl key in
                       H.handle (Uri.with_path_parsed uri path) id meth headers body sock_data
         | _ -> (Logs.debug (fun m -> m "(Api_handler) failure in %s %s" domain Uri.(to_string uri));
                 not_found ()) (* TODO error *)
     end : HANDLER)

  module Meth_map = Map.Make (struct
                        type t = Cohttp.Code.meth
                        let compare : t -> t -> int = Pervasives.compare
                      end)

  let (%) f g x = f (g x)

  let create_ws_handler ?docstring ?(restrict = []) ~path ~query handler : ws_handler =
    let not_allowed id = List.exists (User.equal id) restrict in
    let handler uri id headers body sock_data =
      redirect_if (not_allowed id) @@ handler uri headers body sock_data
    in
    Uri.Dispatcher.make ?docstring ~path ~query handler

  let create_handler ?docstring ?(restrict = []) ~path ~query handler : http_handler =
    let not_allowed id = List.exists (User.equal id) restrict in
    let handler uri id headers body =
      redirect_if (not_allowed id) @@ handler uri headers body
    in
    Uri.Dispatcher.make ?docstring ~path ~query handler

  let create_dispatcher (domain : string)
        (ws_calls : ws_handler list)
        (calls : (Cohttp.Code.meth * http_handler list) list) : (module HANDLER) =
    let open Uri in
    let ws_tbl = List.fold_left (fun acc node -> Dispatcher.add acc node) Dispatcher.empty ws_calls in
    let tbl = List.fold_left (fun acc (meth, calls) ->
                  let disp = List.fold_left (fun acc node -> Dispatcher.add acc node) Dispatcher.empty calls in
                  Meth_map.add meth disp acc)
                Meth_map.empty calls
    in
    (module struct
       let domain = domain
       let handle uri id meth headers body sock_data =
         let default _id  _headers _body =
           Logs.err (fun m -> m "(Api_handler) failure in %s %s" domain (Uri.to_string uri));
           not_found ()
         in
         let default_ws _id _headers _body _sock_data =
           Logs.err (fun m -> m "(Api_handler) failure in %s %s" domain (Uri.to_string uri));
           not_found ()
         in
         if Headers.is_ws headers
         then Dispatcher.dispatch ~default:default_ws ws_tbl uri id headers body sock_data
         else let tbl = Meth_map.find meth tbl in
              Dispatcher.dispatch ~default tbl uri id headers body
     end : HANDLER)
    
end