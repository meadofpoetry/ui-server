open Containers
open Tyxml_js

module Markup = Components_markup.Form_field.Make(Xml)(Svg)(Html)

let x = ref (Unix.time () |> int_of_float)
let get_id = fun () -> incr x; Printf.sprintf "form-input-%d" !x

class ['a] t ?align_end ~(input: 'a) ~label () =

  let for_id = match (input : 'a :> #Widget.input_widget)#input_id with
    | None    -> let id = get_id () in input#set_input_id id; id
    | Some id -> id
  in
  let label = new Widget.t (Markup.Label.create ~for_id ~label ()
                                 |> Tyxml_js.To_dom.of_label) () in
  let elt = Markup.create ?align_end
              ~input:(Widget.to_markup input)
              ~label:(Widget.to_markup label) ()
            |> Tyxml_js.To_dom.of_div in

  object(self)
    inherit Widget.t elt ()
    method label_widget      = label
    method input_widget : 'a = input

    method label       = self#label_widget#text_content |> Option.get_or ~default:""
    method set_label s = self#label_widget#set_text_content s

    initializer
      React.S.map (fun x -> self#label_widget#add_or_remove_class x "color--disabled-on-light")
        input#s_disabled |> self#_keep_s
  end
