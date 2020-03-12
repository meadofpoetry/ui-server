module type USER = sig
  include Api.USER

  val to_string : t -> string
end

module Make (User : USER) : Template_intf.S with type user = User.t
