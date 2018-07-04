open Tyxml_js

module Markup = Components_markup.Button.Make(Xml)(Svg)(Html)

type style = [ `Raised | `Unelevated | `Stroked ]

class t ?typ ?style ?icon ?dense ?compact ?(ripple=false) ~label () =

  let elt = Markup.create ?button_type:typ ?button_style:style
              ?dense ?compact ?icon ~label ()
            |> Tyxml_js.To_dom.of_button in

  object(self)

    inherit Widget.button_widget elt ()

    method button_element : Dom_html.buttonElement Js.t = elt

    method disabled       = Js.to_bool self#button_element##.disabled
    method set_disabled x = self#button_element##.disabled := Js.bool x

    initializer
      if ripple then Ripple.attach self |> ignore

  end
