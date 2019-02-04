open Tyxml_js

module Markup = Components_tyxml.Avatar.Make(Xml)(Svg)(Html)

module Image = struct

end

module Font_icon = struct

end

module Letter = struct

  class t ~text () =
    let elt = Markup.Letter.create ~text () |> To_dom.of_div in
    object
      inherit Widget.t elt () as super

      method dense : bool =
        super#has_class Markup.dense_class

      method set_dense (x : bool) : unit =
        super#add_or_remove_class x Markup.dense_class

    end

end
