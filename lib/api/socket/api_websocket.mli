module Make (User : Api.USER) (Body : Api.BODY) (Msg : Api.WS_BODY with type t = Body.t) : sig

  module Api_http : module type of Api_cohttp.Make (User) (Body)

  type t

  type event_node

  type node = Api_http.node

  type user = Api_http.user

  val event_node : ?doc:string
                   -> ?restrict:user list
                   -> path:('a, 'b)
                        Netlib.Uri.Path.Format.t
                   -> query:('b, user -> Body.t React.event Lwt.t)
                        Netlib.Uri.Query.format
                   -> 'a
                   -> event_node

  val make : ?prefix:string
             -> event_node list
             -> t

  val merge : ?prefix:string
              -> t list
              -> t

  val to_http : ?doc:string -> prefix:string -> ping:unit React.event -> t -> Api_http.t

end
