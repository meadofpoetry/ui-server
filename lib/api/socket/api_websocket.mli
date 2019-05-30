module type CONTROL_MSG = sig
  type t
  val parse : t -> (int * string) option
  val compose : int -> t -> t
end

module Json_msg : CONTROL_MSG with type t = Yojson.Safe.json

module Make (User : Api.USER) (Body : Api.BODY) (Msg : CONTROL_MSG with type t = Body.t) : sig

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

  val to_http_node : ?doc:string -> prefix:string -> t -> node
  (*
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
   *)

end
