(** Type for responses returning a collection of ['a]
 ** [has_more] shows if there is some items left.
 ** A client may do a new request to pull remaining items in case if [has_more] is [true]
 ** [total] is a total number of items in a requested collection. Inserted to a response
 **  in case if a client added corresponding query in a request
 ** [decimation] is a flag indicating if decimation algorithm was applied to the data.
 ** Decimation is applied only if directly requested by the user
 **)
type 'a collection =
  { data       : 'a
  ; has_more   : (bool [@default false])
  ; total      : (int option [@default None])
  ; decimation : (bool [@default false]) (** TODO change to variant or string (algorithm type)? **)
  } [@@deriving yojson]

(** Scheme of a request **)
type scheme = [`WS | `REST]

(** Method of a request **)
type meth   = Cohttp.Code.meth

(** Path of a request **)
type path   = string list

let meth_of_uri (u:Uri.t) = match Uri.scheme u with
  | Some "ws" | Some "wss" -> `WS
  | _                      -> `REST
