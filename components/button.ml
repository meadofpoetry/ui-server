open Tyxml_js
open Containers

module Markup = Components_markup.Button.Make(Xml)(Svg)(Html)

type style = [ `Raised | `Unelevated | `Stroked ]

class t ?typ ?style ?icon ?dense ?compact ?(ripple=false) ~label () =

  let () = Option.iter (fun x -> x#add_class Markup.icon_class) icon in
  let icon = Option.map Widget.to_markup icon in
  let elt = Markup.create ?button_type:typ ?button_style:style
              ?dense ?compact ?icon ~label ()
            |> Tyxml_js.To_dom.of_button in

  object(self)

    val mutable _ripple : Ripple.t option = None

    inherit Widget.button_widget elt () as super

    method button_element : Dom_html.buttonElement Js.t = elt

    method disabled = Js.to_bool self#button_element##.disabled
    method set_disabled x = self#button_element##.disabled := Js.bool x
                          
    method layout () : unit =
      super#layout ();
      Option.iter (fun r -> r#layout ()) _ripple

    method destroy () : unit =
      super#destroy ();
      Option.iter (fun x -> x#destroy ()) _ripple;
      _ripple <- None

    initializer
      if ripple
      then
        let ripple = Ripple.attach_to (self :> Widget.t) in
        _ripple <- Some ripple

  end
