class t ?icon ?ripple ~label () =

  let elt = Markup.Button.create ?icon ?ripple ~label () |> Tyxml_js.To_dom.of_button in
  let e_click,e_click_push = React.E.create () in

  object(self)

    inherit Widget.widget elt () as super

    method private add_or_rm_class x c = if x then super#add_class c else super#remove_class c

    method button_element : Dom_html.buttonElement Js.t = elt

    method set_unelevated x = self#add_or_rm_class x Markup.Button.unelevated_class
    method set_stroked x    = self#add_or_rm_class x Markup.Button.stroked_class
    method set_raised x     = self#add_or_rm_class x Markup.Button.raised_class
    method set_dense x      = self#add_or_rm_class x Markup.Button.dense_class
    method set_compact x    = self#add_or_rm_class x Markup.Button.compact_class

    method get_disabled     = Js.to_bool self#button_element##.disabled
    method set_disabled x   = self#button_element##.disabled := Js.bool x

    method e_click = e_click

    initializer
      Dom_events.listen self#root Dom_events.Typ.click (fun _ _ -> e_click_push (); false) |> ignore;

  end
