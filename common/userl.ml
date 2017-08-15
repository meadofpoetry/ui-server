type typ  = [`Root | `Operator | `Guest ]
          
let typ_of_yojson = function
  | `String "root"     -> Ok `Root
  | `String "operator" -> Ok `Operator
  | `String "guest"    -> Ok `Guest
  | err -> Error ("typ_of_yojson: wrong data" ^ (Yojson.Safe.to_string err))

let typ_to_yojson = function
  | `Root     -> `String "root"
  | `Operator -> `String "operator"
  | `Guest    -> `String "guest"
               
type user = { typ : typ; name : string } [@@deriving yojson]

type full = { user : user ; password : string; email : string option } [@@deriving yojson]
type pass = { user : user ; password : string } [@@deriving yojson]
type info = { user : user ; email : string } [@@deriving yojson]

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
