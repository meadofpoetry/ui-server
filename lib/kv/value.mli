module type S = sig

  type t

  val pp : t Fmt.t

  val equal : t -> t -> bool

  val to_string : t -> string

  val of_string : string -> t
  
end

module String : S with type t = string
