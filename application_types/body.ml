

type t = Yojson.Safe.json

let to_string x = Yojson.Safe.to_string x

let of_string s =
  Ok (Yojson.Safe.from_string s)
