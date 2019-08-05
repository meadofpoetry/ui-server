type 'a raw =
  { data     : 'a
  ; has_more : bool
  ; order    : [ `Asc | `Desc ]
  } [@@deriving yojson]

type ('a,'b) rows =
  | Compressed of 'b
  | Raw of 'a raw [@@deriving yojson]

type _ key = Key : string -> string key | Auth : (string * string) key

type env = { env : 'a. 'a key -> 'a option }

(* TODO elaborate this *)
type 'a response = [ `Value of 'a | `Unit | `Error of string | `Not_implemented]

module Authorize = struct

  type error = [`Need_auth | `Wrong_password | `Unknown of string ]

  let auth validate env =
    env.env Auth
    |> function
      | None ->
         Lwt.return_error `Need_auth
      | Some (name, pass) ->
         validate ~name ~pass

end

type ws_msg_code =
  | ERROR
  | SUBSCRIBE
  | SUBSCRIBED
  | UNSUBSCRIBE
  | UNSUBSCRIBED
  | EVENT

let ws_msg_code_to_enum = function
  | ERROR -> 8
  | SUBSCRIBE -> 32
  | SUBSCRIBED -> 33
  | UNSUBSCRIBE -> 34
  | UNSUBSCRIBED -> 35
  | EVENT -> 36

let ws_msg_code_of_enum = function
  | 8 -> Some ERROR
  | 32 -> Some SUBSCRIBE
  | 33 -> Some SUBSCRIBED
  | 34 -> Some UNSUBSCRIBE
  | 35 -> Some UNSUBSCRIBED
  | 36 -> Some EVENT
  | _ -> None

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
               -> ?forbidden:(user -> response)
               -> ?default:(user -> response)
               -> env:env
               -> redir:(env -> (user, Authorize.error) Lwt_result.t)
               -> path
               -> string
               -> response

end
