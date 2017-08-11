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

module Storage : sig 
  type _ req =
    | Get_info : string -> (int * string) Lwt.t req
    
  include (Database.STORAGE with type 'a req := 'a req)
end = struct
  type _ req =
    | Get_info : string -> (int * string) Lwt.t req

  let request (type a) dbs (r : a req) : a =
    let open Database in
    match r with
    | Get_info name -> Database.select_one dbs
                                           [%sql "SELECT @d{type}, @s{password} FROM users \
                                                  WHERE login = %s"] name
end
