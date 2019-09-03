module Make (Xml : Xml_sigs.NoWrap)
    (Svg : Svg_sigs.NoWrap with module Xml := Xml)
    (Html : Html_sigs.NoWrap with module Xml := Xml
                              and module Svg := Svg) = struct
  module Avatar = struct
    include Avatar
    include Avatar.Make(Xml)(Svg)(Html)
  end

  module Hexdump = struct
    include Hexdump
    include Hexdump.Make(Xml)(Svg)(Html)
  end

  module Pagination = struct
    include Pagination
    include Pagination.Make(Xml)(Svg)(Html)
  end

  module Split = struct
    include Split
    include Split.Make(Xml)(Svg)(Html)
  end

  module Overflow_menu = struct
    include Overflow_menu
    include Overflow_menu.Make(Xml)(Svg)(Html)
  end

  module Placeholder = struct
    include Placeholder
    include Placeholder.Make(Xml)(Svg)(Html)
  end

  module Transform = struct
    include Transform
    include Transform.Make(Xml)(Svg)(Html)
  end
end
