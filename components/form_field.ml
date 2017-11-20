open Widget
open Widget.Widgets.Form_field
open Tyxml_js

class form_label ~for_id ~label = object

  inherit [Dom_html.labelElement Js.t] widget (Label.create ~for_id ~label () |> To_dom.of_label) ()

end

class ['a,'b ] t ?align_end ~(input : 'a) ~label () =

  let label = new form_label
                  ~for_id:((input :> 'b input_widget)#input##.id |> Js.to_string)
                  ~label in

  let elt = create ?align_end
                   ~input:(Tyxml_js.Of_dom.of_element input#root)
                   ~label:(Tyxml_js.Of_dom.of_label label#root) ()
            |> To_dom.of_div in

  object

    inherit [Dom_html.divElement Js.t] widget elt ()

    method private input = input
    method label = label

  end
