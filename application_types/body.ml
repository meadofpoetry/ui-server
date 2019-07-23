type t = Yojson.Safe.t

let to_string x = Yojson.Safe.to_string x

let of_string = function
  | "" -> Ok `Null
  | s ->
    try Ok (Yojson.Safe.from_string s)
    with Yojson.Json_error s -> Error (`Conv_error s)

let content_type =
  "application/json; charset=UTF-8"

let accept =
  "application/json, text/javascript, */*; q=0.01"
