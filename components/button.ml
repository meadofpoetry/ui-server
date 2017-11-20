open Widget
open Widget.Widgets.Button
open Tyxml_js

class t ?raised ?icon ?ripple ~label () =

  let elt = create ?raised ?icon ?ripple ~label () |> To_dom.of_button in

  object(self)

    inherit [Dom_html.buttonElement Js.t] widget elt () as super

    method unelevated     = super#add_class unelevated_class
    method stroked        = super#add_class stroked_class
    method raised         = super#add_class raised_class
    method dense          = super#add_class dense_class
    method compact        = super#add_class compact_class

    method not_unelevated = super#remove_class unelevated_class
    method not_stroked    = super#remove_class stroked_class
    method not_raised     = super#remove_class raised_class
    method not_dense      = super#remove_class dense_class
    method not_compact    = super#remove_class compact_class

    method disabled        = Js.to_bool elt##.disabled
    method disable         = elt##.disabled := Js._true
    method enable          = elt##.disabled := Js._false
    method toggle_disabled = elt##.disabled := Js.bool @@ not self#disabled

  end
