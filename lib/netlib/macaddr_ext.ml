include Macaddr

let equal (a : t) (b : t) = 0 = compare a b

let to_yojson (t:t) : Yojson.Safe.t =
  `String (to_string t)

let of_yojson : Yojson.Safe.t -> (t,string) result = function
  | `String s -> (match of_string s with
                  | Ok _ as m -> m
                  | Error (`Msg m) -> Error ("bad mac: " ^ m))
  | _         -> Error "not a mac address"
