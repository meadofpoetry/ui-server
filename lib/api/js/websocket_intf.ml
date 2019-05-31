module type WS = sig
  open Netlib

  type body

  type t

  val close_socket : t -> unit

  val subscribe : path:('b, 'c) Netlib.Uri.Path.Format.t
    -> query:('c,
              (body -> ('a, string) result) ->
              t ->
              (int * 'a React.event,
               [ `Error of Uri.t * string option
               | `Timeout of float ]) result Lwt.t)
           Netlib.Uri.Query.format
    -> 'b

  val open_socket : ?secure:bool
    -> ?host:string
    -> ?port:int
    -> path:('a, unit -> ((t, string) Lwt_result.t)) Uri.Path.Format.t
    -> 'a
end
