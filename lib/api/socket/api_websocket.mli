
module Make (User : Api.USER) (Body : Api.BODY) : sig

  module Api_http : module type of Api_cohttp.Make (User) (Body)
  
  type node = Api_http.node

  type user = Api_http.user

  type body = Api_http.body

  type state = Api_http.state

  type env = Api.env

  type event = [ `Ev of state * body React.event
               | `Error of string
               ]

  val event : state -> body React.event -> event Lwt.t

  val node : ?doc:string
             -> ?restrict:user list
             -> path:('a, 'b)
                  Netlib.Uri.Path.Format.t
             -> query:('b, user -> body -> env -> state -> event Lwt.t)
                  Netlib.Uri.Query.format
             -> 'a
             -> node

end
