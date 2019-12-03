module Xml : Intf.Xml = struct
  include Tyxml.Xml

  module Wutils = struct
    type step = unit

    let value x = x

    let const x = x

    let tot x = x

    let totlist x = x

    let l2 f x y = f x y

    let create x = x, fun ?step _ -> ignore step
  end
end

module Svg = Svg_f.Make (Xml)
module Html = Html_f.Make (Xml) (Svg)
