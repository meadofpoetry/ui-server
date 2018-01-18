let x = ref (Unix.time () |> int_of_float)
let get_id = fun () -> incr x; Printf.sprintf "form-input-%d" !x

class ['a] t ?align_end ~(input: 'a) ~label () =

  let for_id = match (input : 'a :> #Widget.input_widget)#get_input_id with
    | None -> let id = get_id () in
              input#set_input_id id;
              id
    | Some id -> id
  in
  let label = new Widget.widget (Markup.Form_field.Label.create ~for_id ~label ()
                                 |> Tyxml_js.To_dom.of_label) () in
  let elt = Markup.Form_field.create ?align_end
                                     ~input:(Widget.widget_to_markup input)
                                     ~label:(Widget.widget_to_markup label) ()
            |> Tyxml_js.To_dom.of_div in

  object(self)
    inherit Widget.widget elt ()
    method get_label_widget = label
    method get_input_widget : 'a = input
    method get_label    = self#get_label_widget#get_text_content |> CCOpt.get_or ~default:""
    method set_label s  = self#get_label_widget#set_text_content s

    initializer
      React.S.map (fun x -> "color--disabled-on-light"
                            |> (fun c -> if x then self#get_label_widget#add_class c
                                         else self#get_label_widget#remove_class c)) input#s_disabled |> ignore
  end
