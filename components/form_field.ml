class ['a] t ?align_end ~(input: #Widget.input_widget) ~label () =

  let for_id = ((input :> Widget.input_widget)#input_element##.id |> Js.to_string) in
  let label = new Widget.widget (Markup.Form_field.Label.create ~for_id ~label ()
                                 |> Tyxml_js.To_dom.of_label) () in
  let elt = Markup.Form_field.create ?align_end
                                     ~input:(Widget.widget_to_markup input)
                                     ~label:(Widget.widget_to_markup label) ()
            |> Tyxml_js.To_dom.of_div in

  object(self)
    inherit Widget.widget elt ()
    method get_label_widget = label
    method get_input_widget : Widget.input_widget = (input :> Widget.input_widget)
    method get_label    = self#get_label_widget#get_text_content |> CCOpt.get_or ~default:""
    method set_label s  = self#get_label_widget#set_text_content s
  end
