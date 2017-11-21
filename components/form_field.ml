open Widget
open Markup
open Tyxml_js

class ['a,'b ] t ?align_end ~(input : 'a) ~label () =

  let for_id = ((input :> 'b input_widget)#input##.id |> Js.to_string) in

  let label = new widget (Form_field.Label.create ~for_id ~label () |> To_dom.of_label) () in

  let elt = Form_field.create ?align_end
                              ~input:(Of_dom.of_element input#root)
                              ~label:(Of_dom.of_label label#root) ()
            |> To_dom.of_div in

  object

    inherit [Dom_html.divElement Js.t] widget elt ()

    method input_widget = input
    method label_widget = label

  end
