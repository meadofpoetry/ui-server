open Js_of_ocaml_tyxml

include Components_lab_tyxml.Avatar

module Markup = Make(Tyxml_js.Xml)(Tyxml_js.Svg)(Tyxml_js.Html)

module Image = struct

end

module Font_icon = struct

end

module Letter = struct
  class t ~text () =
    let elt =
      Tyxml_js.To_dom.of_div
      @@ Markup.Letter.create ~text () in
    object
      inherit Components.Widget.t elt () as super

      method dense : bool =
        super#has_class CSS.dense

      method set_dense (x : bool) : unit =
        super#toggle_class ~force:x CSS.dense
    end
end
