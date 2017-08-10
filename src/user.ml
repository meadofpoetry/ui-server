open Containers

let (%) = Fun.(%)
                     
type user = Root | Operator | Guest
                 
let to_int = function
  | Root     -> 0
  | Operator -> 1
  | Guest    -> 2

let of_int = function
  | 0 -> Root
  | 1 -> Operator
  | _ -> Guest

let is_root = function
  | Root -> true
  | _    -> false
