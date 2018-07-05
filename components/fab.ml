open Containers
open Tyxml_js

module Markup = Components_markup.Fab.Make(Xml)(Svg)(Html)

class t ?(ripple=true) ?(mini=false) ~icon () =
  let span = Markup.create_span ~icon () |> Tyxml_js.To_dom.of_element |> Widget.create in
  let elt  = Markup.create ~span:(Widget.to_markup span) () |> Tyxml_js.To_dom.of_button in
  object(self)
    val mutable _ripple = None
    inherit Widget.button_widget elt () as super

    method button_element : Dom_html.buttonElement Js.t = elt

    method mini       = super#has_class Markup.mini_class
    method set_mini x = super#add_or_remove_class x Markup.mini_class

    method icon       = span#text_content
    method set_icon x = span#set_text_content x

    method disabled       = Js.to_bool self#button_element##.disabled
    method set_disabled x = self#button_element##.disabled := Js.bool x

    method layout () = super#layout (); Option.iter (fun r -> r##layout ()) _ripple

    (** Private methods **)

    initializer
      self#set_mini mini;
      if ripple then _ripple <- Some (Ripple.attach self)
  end
