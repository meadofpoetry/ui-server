include module type of Macaddr with type t = Macaddr.t

val equal : t -> t -> bool

val to_yojson : t -> Yojson.Safe.t

val of_yojson : Yojson.Safe.t -> (t, string) result
