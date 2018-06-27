open Containers

let list_to_yojson (f:'a -> Yojson.Safe.json) (l:'a list) : Yojson.Safe.json =
  `List (List.map f l)
let list_of_yojson (f:Yojson.Safe.json -> ('a,string) result) = function
  | `List l -> List.map f l |> List.all_ok
  | _       -> Error "not a list"

let opt_to_yojson (f:'a -> Yojson.Safe.json) = function
  | None   -> `Null
  | Some v -> f v
let opt_of_yojson (f:Yojson.Safe.json -> ('a,string) result) = function
  | `Null -> Ok None
  | json  -> Result.map Option.return @@ f json
