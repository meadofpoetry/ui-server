open Utils

module Make(Xml : Xml_sigs.NoWrap)
         (Svg : Svg_sigs.NoWrap with module Xml := Xml)
         (Html : Html_sigs.NoWrap
          with module Xml := Xml
           and module Svg := Svg) = struct
  open Html

  let base_class               = "mdc-hexdump"
  let block_class              = CSS.add_element base_class "block"
  let chars_block_class        = CSS.add_modifier block_class "chars"
  let line_numbers_block_class = CSS.add_modifier block_class "line-numbers"
  let interactive_class        = CSS.add_modifier base_class "interactive"

  module Line_number = struct
    let _class = CSS.add_element base_class "line-number"
  end

  module Hex = struct
    let _class         = CSS.add_element base_class "hex"
    let empty_class    = CSS.add_element base_class "hex-empty"
    let selected_class = CSS.add_modifier _class "selected"
  end

  module Char = struct
    let _class         = CSS.add_element base_class "char"
    let empty_class    = CSS.add_element base_class "char-empty"
    let selected_class = CSS.add_modifier _class "selected"
  end

  let create_block ?(classes=[]) ?attrs () =
    pre ~a:([ a_class (block_class :: classes)] <@> attrs) []

end
