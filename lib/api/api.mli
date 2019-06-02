type 'a raw =
  { data     : 'a
  ; has_more : bool
  ; order    : [ `Asc | `Desc ]
  }

type ('a,'b) rows =
  | Compressed of 'b
  | Raw        of 'a raw

val raw_to_yojson : ('a -> Yojson.Safe.json) -> 'a raw -> Yojson.Safe.json

val raw_of_yojson : (Yojson.Safe.json -> ('a, string) result)
                    -> Yojson.Safe.json
                    -> ('a raw, string) result

val rows_to_yojson : ('a -> Yojson.Safe.json)
                     -> ('b -> Yojson.Safe.json)
                     -> ('a, 'b) rows
                     -> Yojson.Safe.json

val rows_of_yojson : (Yojson.Safe.json -> ('a, string) result)
                     -> (Yojson.Safe.json -> ('b, string) result)
                     -> Yojson.Safe.json
                     -> (('a,'b) rows, string) result

type _ key = Key : string -> string key | Auth : (string * string) key

type env = { env : 'a. 'a key -> 'a option }

(* TODO elaborate this *)
type 'a response = [ `Value of 'a | `Unit | `Error of string | `Not_implemented]

module Authorize : sig

  type error = [`Need_auth | `Wrong_password | `Unknown of string ]

  val auth : (name:string -> pass:string -> ('id, [> error ] as 'b) Lwt_result.t)
             -> env
             -> ('id, 'b) Lwt_result.t

end

type ws_msg_code =
  | ERROR
  | SUBSCRIBE
  | SUBSCRIBED
  | UNSUBSCRIBE
  | UNSUBSCRIBED
  | EVENT

val ws_msg_code_to_enum : ws_msg_code -> int

val ws_msg_code_of_enum : int -> ws_msg_code option

module type USER = sig
  type t
  val equal : t -> t -> bool
end

module type BODY = sig
  type t
  val to_string : t -> string
  val of_string : string -> (t, [>`Conv_error of string]) result
  val content_type : string
end

type 'a ws_message =
  [ `Subscribe of string
  | `Subscribed of int
  | `Unsubscribe of int
  | `Unsubscribed
  | `Event of 'a
  | `Error of string
  ]

module type WS_BODY = sig
  type t
  val parse : t -> (int * t ws_message) option
  val compose : int -> t ws_message -> t
end

module type S = sig

  type t

  type state
     
  type user

  type path
     
  type body

  type meth

  type answer

  type response

  type 'a handler

  type node = (user -> string -> env -> state -> answer Lwt.t) handler

  (* raises Ambiguity on ambiguous path *)
  val merge : ?prefix:string
              -> t list
              -> t
    
  val handle : t
               -> state:state
               -> ?meth:meth
               -> ?default:(unit -> response)
               -> env:env
               -> redir:(env -> (user, Authorize.error) Lwt_result.t)
               -> path
               -> string
               -> response
    
end
