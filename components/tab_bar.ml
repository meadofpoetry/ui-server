open Containers
open Tyxml_js

module Markup = Components_markup.Tab_bar.Make(Xml)(Svg)(Html)

class ['a,'b] t ?align ~(tabs:('a,'b) Tab.t list) () =
  let scroller = new Tab_scroller.t ?align ~tabs () in
  let elt = Markup.create ~scroller:(Widget.to_markup scroller) ()
            |> To_dom.of_element in

  object(self)

    inherit Widget.t elt ()

    method scroller : ('a,'b) Tab_scroller.t = scroller

  end
