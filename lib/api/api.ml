type 'a raw =
  { data     : 'a
  ; has_more : bool
  ; order    : [ `Asc | `Desc ]
  } [@@deriving yojson]

type ('a,'b) rows =
  | Compressed of 'b
  | Raw        of 'a raw
  [@@deriving yojson]

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

module Json_body : BODY with type t = Yojson.Safe.json = struct
  type t = Yojson.Safe.json

  let to_string x = Yojson.Safe.to_string x

  let of_string = function
    | "" -> Ok `Null
    | s ->
      try Ok (Yojson.Safe.from_string s)
      with Yojson.Json_error s -> Error (`Conv_error s)

  let content_type =
    "application/json; charset=UTF-8"

  let accept =
    "application/json, text/javascript, */*; q=0.01"
end
