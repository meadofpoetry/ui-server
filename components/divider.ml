open Containers
open Tyxml_js

module Markup = Components_markup.Divider.Make(Xml)(Svg)(Html)

class t ?inset () =
  let elt = To_dom.of_element @@ Markup.create () in
  object(self)

    inherit Widget.t elt () as super

    method! init () : unit =
      super#init ();
      Option.iter self#set_inset inset

    method set_inset (x : bool) : unit =
      self#add_or_remove_class x Markup.inset_class

    method inset : bool =
      self#has_class Markup.inset_class

  end
