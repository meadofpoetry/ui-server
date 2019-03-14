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
type 'a response = [ `Body of 'a | `Ok | `Error of string ]

module Authorize = struct

  type error = [`Need_auth | `Wrong_password]

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

  type node = (user -> body -> env -> state -> answer) handler
            
  val merge : domain:string
              -> t list
              -> t
    
  val handle : t
               -> state:state
               -> ?meth:meth
               -> env:env
               -> redir:((user -> response) -> response)
               -> path
               -> string
               -> response
    
end
