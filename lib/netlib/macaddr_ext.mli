include module type of Macaddr with type t = Macaddr.t

val equal : t -> t -> bool

val to_yojson : t -> Yojson.Safe.json

val of_yojson : Yojson.Safe.json -> (t, string) result
