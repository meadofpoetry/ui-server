type t = Yojson.Safe.json

let to_string x = Yojson.Safe.to_string x

let of_string = function
  | "" -> Ok `Null
  | s ->
    try Ok (Yojson.Safe.from_string s)
    with Yojson.Json_error s -> Error (`Conv_error s)
