(* TODO add ripple manually, without auto-init *)

class t ?(ripple=true) ?(mini=false) ~icon () =
  let elt = Markup.Fab.create ~icon () |> Tyxml_js.To_dom.of_button in
  object(self)
    inherit Widget.button_widget elt () as super

    method button_element : Dom_html.buttonElement Js.t = elt

    method mini       = super#has_class Markup.Fab.mini_class
    method set_mini x = super#add_or_remove_class x Markup.Fab.mini_class

    method disabled       = Js.to_bool self#button_element##.disabled
    method set_disabled x = self#button_element##.disabled := Js.bool x

    initializer
      self#set_mini mini;
      if ripple then Ripple.attach self |> ignore
  end
