module Xml = struct
  include Js_of_ocaml_tyxml.Tyxml_js.Xml

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

module Svg = Js_of_ocaml_tyxml.Tyxml_js.Svg
module Html = Js_of_ocaml_tyxml.Tyxml_js.Html

module R = struct
  module Xml = struct
    include Js_of_ocaml_tyxml.Tyxml_js.R.Xml

    module Wutils = struct
      type step = React.step

      let value = React.S.value

      let const = ReactiveData.RList.const

      let tot x = ReactiveData.RList.signal ?eq:None x

      let totlist x = ReactiveData.RList.from_signal ?eq:None x

      let l2 f x y = React.S.l2 f x y

      let create x = React.S.create ?eq:None x
    end
  end

  module Svg = Js_of_ocaml_tyxml.Tyxml_js.R.Svg
  module Html = Js_of_ocaml_tyxml.Tyxml_js.R.Html
end
