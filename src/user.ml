open Lwt
open Containers
open Cryptokit

module Tokentbl = Hashtbl.Make(String)

let b64 = Cryptokit.Base64.encode_compact ()
                
let day   = 86400.
let month = day *. 28. (* seconds in month *)
                
type user = Root | Guest

type token = (user * float)

type token_table = token Tokentbl.t             
                 
let to_string = function
  | Root  -> "root"
  | Guest -> "guest"

let of_string = function
  | "root" -> Root
  | _      -> Guest

let is_expired : token -> bool = function
  | (_,tm) -> (Unix.time ()) > tm

let is_root : token -> bool = function
  | (Root,_) -> true
  | _        -> false

let get_token usr : token = (usr, (Unix.time () +. month))
                  
let hash_token hsh : token -> string = function
  | (usr,tm) -> transform_string b64 @@ hash_string hsh (to_string usr ^ "|" ^ string_of_float tm)
