
module Make (User : Api.USER) (Body : Api.BODY) : sig

  module Api_http : module type of Api_cohttp.Make (User) (Body)
  
  type node = Api_http.node

  type user = Api_http.user

  type body = Api_http.body

  type state = Api_http.state

  type env = Api.env

  type event = [ `Ev of body React.event
               | `Error of string
               ]

  type socket_table

  val make_socket_table : unit -> socket_table

  val close_sockets : socket_table -> unit
    
  val event : body React.event -> event Lwt.t

  val node : ?doc:string
             -> ?restrict:user list
             -> socket_table:socket_table
             -> path:('a, 'b)
                  Netlib.Uri.Path.Format.t
             -> query:('b, user -> body -> env -> state -> event Lwt.t)
                  Netlib.Uri.Query.format
             -> 'a
             -> node

end
