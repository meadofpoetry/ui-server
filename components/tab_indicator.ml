open Containers
open Tyxml_js

module Markup = Components_markup.Tab_indicator.Make(Xml)(Svg)(Html)

class t ?(fade=false) ?(active=false) () =
  let elt = Markup.create () |> To_dom.of_element in
  object(self)
    inherit Widget.t elt ()

    method fade : bool =
      self#has_class Markup.fade_class
    method set_fade (x:bool) : unit =
      self#add_or_remove_class x Markup.fade_class

    method active : bool =
      self#has_class Markup.active_class
    method set_active (x:bool) : unit =
      self#add_or_remove_class x Markup.active_class
  end
