open Utils

module Make(Xml : Xml_sigs.NoWrap)
         (Svg : Svg_sigs.NoWrap with module Xml := Xml)
         (Html : Html_sigs.NoWrap
          with module Xml := Xml
           and module Svg := Svg) = struct
  open Html

  let base_class = "mdc-hexdump"

  module Line_number = struct
    let _class = CSS.add_element base_class "line-number"
  end

  module Hex = struct
    let _class = CSS.add_element base_class "hex"
  end

  module Char = struct
    let _class = CSS.add_element base_class "char"
  end

  let create ?(classes=[]) ?attrs contents () =
    pre ~a:([ a_class (base_class :: classes)] <@> attrs) contents

end
