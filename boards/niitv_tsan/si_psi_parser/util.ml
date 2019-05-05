module Bitstring = struct

  include Bitstring

  let ( % ) f g x = f (g x)

  let of_string = bitstring_of_string

  let to_string = string_of_bitstring

  let to_cstruct = Cstruct.of_string % string_of_bitstring

  let length = bitstring_length

end
