open Widget
open Markup
open Tyxml_js

(* TODO add ripple manually, without auto-init *)

class t ?ripple ?(mini=false) ~icon () =
  let elt = Fab.create ?ripple ~icon () |> To_dom.of_button in
  object(self)
    inherit widget elt () as super
    method button_element : Dom_html.buttonElement Js.t = elt

    method set_mini x = super#add_or_remove_class x Markup.Fab.mini_class
    method get_mini   = super#has_class Markup.Fab.mini_class

    initializer
      self#set_mini mini
  end
