open Containers
open Tyxml_js

module Markup = Components_tyxml.Fab.Make(Xml)(Svg)(Html)

class t ?(ripple = true) ?(mini = false) ~icon () =
  let elt = Markup.create ~icon:(Widget.to_markup icon) ()
            |> To_dom.of_button in
  object(self)
    val mutable _ripple : Ripple.t option = None

    inherit Widget.button_widget elt () as super

    method! init () : unit =
      super#init ();
      icon#add_class Markup.icon_class;
      self#set_mini mini;
      if ripple then
        let ripple = Ripple.attach super#root in
        _ripple <- Some ripple

    method! layout () : unit =
      super#layout ();
      Option.iter (fun r -> r#layout ()) _ripple

    method! destroy () : unit =
      super#destroy ();
      Option.iter (fun r -> r#destroy ()) _ripple;
      _ripple <- None

    method mini : bool =
      super#has_class Markup.mini_class

    method set_mini (x : bool) : unit =
      super#toggle_class ~force:x Markup.mini_class

  end
