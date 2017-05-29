open Lwt
open Containers

let (%) = Fun.(%)
                     
type user = Root | Guest
                 
let to_string = function
  | Root  -> "root"
  | Guest -> "guest"

let of_string = function
  | "root" -> Root
  | _      -> Guest

let is_root = function
  | Root -> true
  | _    -> false
