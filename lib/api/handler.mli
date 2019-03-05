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

module Make (User : USER) : S with type user := User.t
