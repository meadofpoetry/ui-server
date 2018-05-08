open Containers

type user = [ `Root | `Operator | `Guest ]

let user_to_string = function
  | `Root     -> "root"
  | `Operator -> "operator"
  | `Guest    -> "guest"

let user_of_string = function
  | "root"     -> Some `Root
  | "operator" -> Some `Operator
  | "guest"    -> Some `Guest
  | _          -> None

type key  = string [@@deriving yojson,eq]

type data = (key * Yojson.Safe.json) list [@@deriving yojson]

type root =
  { root     : data
  ; operator : data
  ; guest    : data
  } [@@deriving yojson]

module type Store = sig
  val put    : key -> Yojson.Safe.json -> unit
  val get    : key -> Yojson.Safe.json option
  val remove : key -> unit
end

module Make(M:Store) = struct

  let get_user () =
    let s = Js.Unsafe.variable "username" in
    match user_of_string s with
    | Some u -> u
    | None   -> failwith @@ Printf.sprintf "Unknown user type: %s" s

  let put (key:key) (data:Yojson.Safe.json) : unit =
    let user = get_user () in
    let upd  = match M.get @@ user_to_string user with
      | Some (`Assoc l) -> (key,data) :: l
      | _               -> [key,data]
    in
    M.put (user_to_string user) (`Assoc upd)

  let get (key:key) : Yojson.Safe.json option =
    let user = get_user () in
    match M.get @@ user_to_string user with
    | Some (`Assoc l) -> List.Assoc.get ~eq:equal_key key l
    | _               -> None

  let remove (key:key) : unit =
    let user = get_user () in
    let upd = match M.get @@ user_to_string user with
      | Some (`Assoc l) -> List.Assoc.remove ~eq:equal_key key l
      | _               -> []
    in
    M.put (user_to_string user) (`Assoc upd)

  let clear ()                =
    let user = get_user () in
    M.remove @@ user_to_string user

end
