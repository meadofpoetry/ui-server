open Widget
open Markup
open Tyxml_js

(* TODO add ripple manually, without auto-init *)

class t ?ripple ~icon () =
  let elt = Fab.create ?ripple ~icon () |> To_dom.of_button in
  object
    inherit widget elt () as super
    method button_element : Dom_html.buttonElement Js.t = elt
    method mini     = super#add_class Fab.mini_class
    method not_mini = super#remove_class Fab.mini_class
  end
