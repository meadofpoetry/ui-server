(*

type t = { } [@@deriving yojson]

let default = { }

let domain = "pipeline"

let of_string x =
  Yojson.Safe.from_string x
  |> of_yojson
  |> function Ok v -> v | Error e -> failwith e

 *)
