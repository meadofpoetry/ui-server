open Lwt
open Containers
open Nocrypto

let (%) = Fun.(%)
   
module Tokentbl = Hashtbl.Make(String)

let cstr_wrap f = Cstruct.to_string % (f % Cstruct.of_string)
                
let hash    = cstr_wrap @@ Hash.mac `SHA1 ~key:(Cstruct.of_string "todo:add_magick_string43")
let b64_enc = cstr_wrap @@ Base64.encode
                    
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
  | (usr,tm) -> b64_enc @@ hash (to_string usr ^ "|" ^ string_of_float tm)
