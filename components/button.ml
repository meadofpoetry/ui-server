open Markup
open Tyxml_js

class t ?raised ?icon ?ripple ~label () =

  let elt = Button.create ?raised ?icon ?ripple ~label () |> To_dom.of_button in

  object(self)

    inherit Widget.widget elt () as super

    method button_element : Dom_html.buttonElement Js.t = elt

    method unelevated     = super#add_class Button.unelevated_class
    method stroked        = super#add_class Button.stroked_class
    method raised         = super#add_class Button.raised_class
    method dense          = super#add_class Button.dense_class
    method compact        = super#add_class Button.compact_class

    method not_unelevated = super#remove_class Button.unelevated_class
    method not_stroked    = super#remove_class Button.stroked_class
    method not_raised     = super#remove_class Button.raised_class
    method not_dense      = super#remove_class Button.dense_class
    method not_compact    = super#remove_class Button.compact_class

    method disabled        = Js.to_bool self#button_element##.disabled
    method disable         = self#button_element##.disabled := Js._true
    method enable          = self#button_element##.disabled := Js._false
    method toggle_disabled = self#button_element##.disabled := Js.bool @@ not self#disabled

  end
