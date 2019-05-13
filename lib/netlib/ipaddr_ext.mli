module V4 : sig
  include module type of Ipaddr.V4 with type t = Ipaddr.V4.t

  val equal : t -> t -> bool

  val to_yojson : t -> Yojson.Safe.json

  val of_yojson : Yojson.Safe.json -> (t, string) result

  val zero : t

  val succ : t -> t

  val in_range : (t * t) -> t -> bool

  val range_to_seq : (t * t) list -> t Seq.t

  val gen_in_ranges : ?forbidden:t list -> ('a * (t * t) list) list -> ('a * t) list

end

module V6 : sig
  include module type of Ipaddr.V6 with type t = Ipaddr.V6.t

  val equal : t -> t -> bool

  val to_yojson : t -> Yojson.Safe.json

  val of_yojson : Yojson.Safe.json -> (t, string) result           

end       

include module type of struct include Ipaddr end
                       with module V4 := Ipaddr.V4
                       with module V6 := Ipaddr.V6
