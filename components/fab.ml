open Widget
open Widget.Widgets.Fab
open Tyxml_js

class t ?ripple ~icon () =

  let elt = create ?ripple ~icon () |> To_dom.of_button in

  object

    inherit [Dom_html.buttonElement Js.t] widget elt () as super

    method mini     = super#add_class mini_class
    method not_mini = super#remove_class mini_class

  end
