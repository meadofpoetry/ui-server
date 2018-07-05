open Tyxml_js

module Markup = Components_markup.Avatar.Make(Xml)(Svg)(Html)

module Image = struct

end

module Font_icon = struct

end

module Letter = struct

  class t ~text () =
    let elt = Markup.Letter.create ~text () |> Tyxml_js.To_dom.of_div in
    object(self)
      inherit Widget.widget elt () as super

      method dense       = self#has_class Markup.dense_class
      method set_dense x = self#add_or_remove_class x Markup.dense_class
    end

end
