open Redirect

let is_ws headers = match Cohttp.Header.get headers "upgrade" with
  | Some "websocket" -> true
  | _                -> false

module type USER = sig
  type t
  val equal : t -> t -> bool
end

module type S = sig
  
  type t
     
  type user

  type meth = Cohttp.Code.meth

  type header = Cohttp.Header.t

  type body = Cohttp_lwt.Body.t
            
  type socket_data = Cohttp_lwt_unix.Request.t * Conduit_lwt_unix.flow

  type http_handler =
    (user -> header -> body -> Interaction.response) Netlib.Uri.Dispatcher.node

  type ws_handler =
    (user -> header -> body -> socket_data -> Interaction.response) Netlib.Uri.Dispatcher.node
                      
  val create_handler : ?docstring:string
                       -> ?restrict:user list
                       -> path:('a -> user -> 'b -> 'c -> Interaction.response, 'd)
                            Netlib.Uri.Path.Format.t
                       -> query:('d, user -> header -> body -> Interaction.response)
                            Netlib.Uri.Query.format
                       -> ('a -> 'b -> 'c -> unit -> Interaction.response)
                       -> http_handler

  val create_ws_handler : ?docstring:string
                          -> ?restrict:user list
                          -> path:('a -> user -> 'b -> 'c -> 'd -> Interaction.response, 'e)
                               Netlib.Uri.Path.Format.t
                          -> query:('e, user -> header -> body -> socket_data -> Interaction.response)
                               Netlib.Uri.Query.format
                          -> ('a -> 'b -> 'c -> 'd -> unit -> Interaction.response)
                          -> ws_handler

  val create_dispatcher : domain:string
                          -> ws_handler list
                          -> (meth * http_handler list) list
                          -> t

  val handle : t
               -> ((user -> Interaction.response) -> Interaction.response)
               -> Netlib.Uri.t
               -> meth
               -> header
               -> body
               -> socket_data
               -> Interaction.response

  val create : t list -> t

  val add_layer : domain:string
                  -> t list
                  -> t
                   
end

module Make ( User : USER ) : S with type user := User.t = struct
  
  open Netlib

  module Meth_map = Map.Make (struct
                        type t = Cohttp.Code.meth
                        let compare : t -> t -> int = Pervasives.compare
                      end)

  type meth = Cohttp.Code.meth

  type header = Cohttp.Header.t

  type body = Cohttp_lwt.Body.t
     
  type socket_data = Cohttp_lwt_unix.Request.t * Conduit_lwt_unix.flow

  type http_handler =
    (User.t -> header -> body -> Interaction.response) Uri.Dispatcher.node

  type ws_handler =
    (User.t -> header -> body -> socket_data -> Interaction.response) Uri.Dispatcher.node
    
  (* http and socket handler *)
  type t = (User.t -> header -> body -> Interaction.response) Uri.Dispatcher.t Meth_map.t *
           (User.t -> header -> body -> socket_data -> Interaction.response) Uri.Dispatcher.t
                   
  let create (hndls : t list) : t =
    let flat _meth l r =
      match l, r with
      | Some tl, Some h -> Some (h::tl)
      | (Some _ as v), None -> v
      | None, Some h -> Some [h]
      | None, None -> None
    in
    let http =
      List.map fst hndls
      |> List.fold_left (Meth_map.merge flat) Meth_map.empty
      |> Meth_map.map Uri.Dispatcher.merge_unsafe
    in
    let ws = Uri.Dispatcher.merge_unsafe (List.map snd hndls) in
    http, ws 
    
  let add_layer ~(domain : string) (l : t list) : t =
    let path = Uri.Path.of_string domain in
    let flat _meth l r =
      match l, r with
      | Some tl, Some h -> Some (h::tl)
      | (Some _ as v), None -> v
      | None, Some h -> Some [h]
      | None, None -> None
    in
    let http =
      List.map fst l
      |> List.fold_left (Meth_map.merge flat) Meth_map.empty
      |> Meth_map.map (fun met ->
             Uri.Dispatcher.(merge empty (List.map (fun x -> path, x) met)))
    in
    let ws = Uri.Dispatcher.merge_unsafe (List.map snd l) in
    http, ws
     
  let handle tbl redir uri meth headers body sock_data =
    let tbl, ws_tbl = tbl in
    let default _id  _headers _body =
      Logs.err (fun m -> m "(Api_handler) failure in %s" (Uri.to_string uri));
      not_found ()
    in
    let default_ws _id _headers _body _sock_data =
      Logs.err (fun m -> m "(Api_handler) failure in %s" (Uri.to_string uri));
      not_found ()
    in
    redir @@ (fun id ->
      if is_ws headers
      then Uri.Dispatcher.dispatch ~default:default_ws ws_tbl uri id headers body sock_data
      else let tbl = Meth_map.find meth tbl in
           Uri.Dispatcher.dispatch ~default tbl uri id headers body) 

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

  let create_dispatcher ~(domain : string)
        (ws_calls : ws_handler list)
        (calls : (Cohttp.Code.meth * http_handler list) list) : t =
    let open Uri in
    let path = Path.of_string domain in
    let ws_tbl =
      List.fold_left (fun acc node -> Dispatcher.add acc (Dispatcher.prepend path node))
        Dispatcher.empty ws_calls
    in
    let tbl =
      List.fold_left (fun acc (meth, calls) ->
          let disp = List.fold_left (fun acc node -> Dispatcher.add acc (Dispatcher.prepend path node))
                       Dispatcher.empty calls
          in
          Meth_map.add meth disp acc)
        Meth_map.empty calls
    in
    tbl, ws_tbl 
    
end
