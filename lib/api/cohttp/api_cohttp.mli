
type env = Api.env

type 'a response = 'a Api.response

val env_of_headers : Cohttp.Header.t -> env

module Redirect = Redirect
                
module Make (User : Api.USER) (Body : Api.BODY) : sig

  include Api.S
          with type state = Cohttp_lwt_unix.Request.t * Conduit_lwt_unix.flow
           and type user = User.t
           and type body = Body.t
           and type meth = Cohttp.Code.meth
           and type path = Netlib.Uri.t
           and type answer = [ Api.Authorize.error
                             | Body.t response
                             | `Instant of (Cohttp.Response.t * Cohttp_lwt.Body.t) Lwt.t
                             ]
           and type response = (Cohttp.Response.t * Cohttp_lwt.Body.t) Lwt.t
           and type 'a handler =
                      Cohttp.Code.meth * 'a Netlib.Uri.Dispatcher.node

  (* raises Ambiguity on ambiguous path *)
  val make : ?prefix:string
             -> node list
             -> t

  val node : ?doc:string
             -> ?restrict:user list
             -> meth:meth
             -> path:('a, 'b)
                  Netlib.Uri.Path.Format.t
             -> query:('b, user -> body -> env -> state -> answer Lwt.t)
                  Netlib.Uri.Query.format
             -> 'a
             -> node

  val doc : t -> (string * string list) list

end
