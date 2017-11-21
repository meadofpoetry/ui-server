open Widget
open Markup
open Tyxml_js

class t ?ripple ~icon () =

  let elt = Fab.create ?ripple ~icon () |> To_dom.of_button in

  object

    inherit [Dom_html.buttonElement Js.t] widget elt () as super

    method mini     = super#add_class Fab.mini_class
    method not_mini = super#remove_class Fab.mini_class

  end
