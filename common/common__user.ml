type t = [`Root | `Operator | `Guest ]

let of_string = function
  | "root"     -> Ok `Root
  | "operator" -> Ok `Operator
  | "guest"    -> Ok `Guest
  | _ -> Error "Unknown user"

let to_string = function
  | `Root     -> "root"
  | `Operator -> "operator"
  | `Guest    -> "guest"
       
let of_yojson = function
  | `String s -> of_string s
  | err -> Error ("typ_of_yojson: wrong data" ^ (Yojson.Safe.to_string err))

let to_yojson u = `String (to_string u)

type pass = { user     : t
            ; password : string
            } [@@deriving yojson]
          
let to_int = function
  | `Root     -> 0
  | `Operator -> 1
  | `Guest    -> 2

let of_int = function
  | 0 -> `Root
  | 1 -> `Operator
  | _ -> `Guest
          
let is_root = function
  | `Root -> true
  | _    -> false

let eq usr1 usr2 =
  match usr1, usr2 with
  | `Root, `Root | `Operator, `Operator | `Guest, `Guest -> true
  | _ -> false
