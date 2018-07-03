include Macaddr

let to_yojson (t:t) : Yojson.Safe.json =
  `String (to_string t)

let of_yojson : Yojson.Safe.json -> (t,string) result = function
  | `String s -> (match of_string s with
                  | Some m -> Ok m
                  | None   -> Error ("bad mac: " ^ s))
  | _         -> Error "not a mac address"
