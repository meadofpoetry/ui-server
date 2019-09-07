type t = int

let max_int = 0xFFFF

let zero = 0

let equal = ( = )

let compare = Stdlib.compare

let add x y = (x + y) land max_int

let sub x y = (x - y) land max_int

let succ x = add x 1

let pred x = sub x 1

let pp ppf = Format.pp_print_int ppf

external to_int : t -> int = "%identity"

let of_int (x : int) : t = x land max_int
