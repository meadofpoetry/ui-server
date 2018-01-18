open Widget
open Markup
open Tyxml_js

(* TODO add ripple manually, without auto-init *)

class t ?ripple ~icon () =
  let elt = Fab.create ?ripple ~icon () |> To_dom.of_button in
  object
    inherit widget elt () as super
    method button_element : Dom_html.buttonElement Js.t = elt
    method set_mini x = Fab.mini_class |> (fun c -> if x then super#add_class c else super#remove_class c)
  end
