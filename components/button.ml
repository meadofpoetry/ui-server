class t ?(typ:[ `Submit | `Button | `File | `Reset ] option) ?icon ?ripple ~label () =

  let elt = Markup.Button.create ?icon ?ripple ~label () |> Tyxml_js.To_dom.of_button in
  let e_click,e_click_push = React.E.create () in

  object(self)

    inherit Widget.widget elt () as super

    method button_element : Dom_html.buttonElement Js.t = elt

    method set_unelevated x = super#add_or_remove_class x Markup.Button.unelevated_class
    method set_stroked x    = super#add_or_remove_class x Markup.Button.stroked_class
    method set_raised x     = super#add_or_remove_class x Markup.Button.raised_class
    method set_dense x      = super#add_or_remove_class x Markup.Button.dense_class
    method set_compact x    = super#add_or_remove_class x Markup.Button.compact_class

    method get_disabled     = Js.to_bool self#button_element##.disabled
    method set_disabled x   = self#button_element##.disabled := Js.bool x

    method e_click = e_click

    initializer
      (match typ with
       | None   -> ()
       | Some x -> super#set_attribute "type" (match x with
                                               | `Submit -> "submit"
                                               | `Button -> "button"
                                               | `File   -> "file"
                                               | `Reset  -> "reset"));
      Dom_events.listen self#root Dom_events.Typ.click (fun _ _ -> e_click_push (); false) |> ignore;

  end
