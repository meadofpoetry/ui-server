open Widget
open Markup
open Tyxml_js

class t ?align_end ~(input : #input_widget) ~label () =

  let for_id = ((input :> input_widget)#input_element##.id |> Js.to_string) in
  let label = new widget (Form_field.Label.create ~for_id ~label () |> To_dom.of_label) () in
  let elt = Form_field.create ?align_end
                              ~input:(widget_to_markup input)
                              ~label:(widget_to_markup label) ()
            |> To_dom.of_div in

  object(self)
    inherit widget elt ()
    method label_widget = label
    method input_widget : input_widget = (input :> input_widget)
    method label        = self#label_widget#text_content
    method set_label s  = self#label_widget#set_text_content s
  end
