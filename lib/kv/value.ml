module type S = sig

  type t

  val pp : t Fmt.t

  val equal : t -> t -> bool

  val to_string : t -> string

  val of_string : string -> t
  
end

module String : (S with type t = string) = struct
  type t = string

  let pp = Fmt.string

  let equal = String.equal
         
  let to_string x = x

  let of_string x = x
end
