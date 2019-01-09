open Js_of_ocaml
open Containers
open Tyxml_js

module Markup = Components_markup.Form_field.Make(Xml)(Svg)(Html)

let x = ref (Unix.time () |> int_of_float)
let get_id = fun () -> incr x; Printf.sprintf "form-input-%d" !x

class ['a] t ?align_end ~(input : 'a) ~label () =

  let for_id = match (input : 'a :> #Widget.input_widget)#input_id with
    | None -> let id = get_id () in input#set_input_id id; id
    | Some id -> id in
  let label = new Widget.t (Markup.create_label ~for_id ~label ()
                            |> To_dom.of_label) () in
  let (elt : Dom_html.element Js.t) =
    Markup.create ?align_end
      ~input:(Widget.to_markup input)
      ~label:(Widget.to_markup label) ()
    |> To_dom.of_div in

  object(self)
    inherit Widget.t elt ()

    method label_widget = label
    method input_widget : 'a = input

    method label : string =
      self#label_widget#text_content
      |> Option.get_or ~default:""

    method set_label (s : string) : unit =
      self#label_widget#set_text_content s

  end
