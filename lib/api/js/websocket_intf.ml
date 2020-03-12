type error = [`Msg of string]

exception Error of error

module type WS = sig
  open Netlib

  type body

  type t

  val subscribe :
       ?reqid:int
    -> path:('a, 'b) Uri.Path.Format.t
    -> query:
         ( 'b
         ,    (body -> ('c, string) result)
           -> t
           -> (int * 'c React.event, error) result Lwt.t )
         Uri.Query.format
    -> 'a

  val unsubscribe : ?reqid:int -> t -> int -> (unit, error) result Lwt.t

  val open_socket :
       ?secure:bool
    -> ?host:string
    -> ?port:int
    -> path:('a, unit -> (t, error) Lwt_result.t) Uri.Path.Format.t
    -> 'a

  val close_socket : ?code:int -> ?reason:string -> t -> unit
end
