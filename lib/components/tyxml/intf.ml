module type Xml = sig
  include Xml_sigs.T with type ('a, 'b) W.ft = 'a -> 'b

  module Wutils : sig
    type step

    val value : 'a W.t -> 'a

    val const : 'a list -> 'a W.tlist

    val tot : 'a W.tlist -> 'a list W.t

    val totlist : 'a list W.t -> 'a W.tlist

    val l2 : ('a -> 'b -> 'c) -> 'a W.t -> 'b W.t -> 'c W.t

    val create : 'a -> 'a W.t * (?step:step -> 'a -> unit)
  end
end
