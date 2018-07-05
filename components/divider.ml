open Tyxml_js

module Markup = Components_markup.Divider.Make(Xml)(Svg)(Html)

class t ?(inset=false) () =
  let elt = Markup.create () |> Tyxml_js.To_dom.of_element in
  object(self)

    inherit Widget.t elt ()

    method set_inset x = self#add_or_remove_class x Markup.inset_class
    method inset       = self#has_class Markup.inset_class

    initializer
      self#set_inset inset
  end
