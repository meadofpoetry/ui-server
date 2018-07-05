open Containers
open Tyxml_js

module Markup = Components_markup.Item_list.Make(Xml)(Svg)(Html)

module Divider = struct
  class t ?inset () = object
    inherit Widget.t (Markup.Item.create_divider ?inset () |> Tyxml_js.To_dom.of_element) ()
  end
end

module Item = struct

  (* TODO add ripple manually, without auto-init *)
  class t ?(ripple=false) ?secondary_text ?graphic ?meta ~text () =
    let text_elt = match secondary_text with
      | Some st -> let secondary = Markup.Item.create_secondary_text st () in
                   Markup.Item.create_text ~secondary text ()
      | None -> Markup.Item.create_text_simple text ()
    in
    let elt = Markup.Item.create
                ?graphic:(Option.map Widget.to_markup graphic)
                ?meta:(Option.map Widget.to_markup meta)
                text_elt ()
              |> Tyxml_js.To_dom.of_element in

    object(self)
      inherit Widget.t elt ()

      (* TODO add setters, real getters *)
      method text           = text
      method secondary_text = secondary_text

      initializer
        if ripple then Ripple.attach self |> ignore;
        Option.iter (fun x -> x#add_class Markup.Item.graphic_class) graphic;
        Option.iter (fun x -> x#add_class Markup.Item.meta_class) meta
    end

end

class t ?avatar ~(items:[ `Item of Item.t | `Divider of Divider.t ] list) () =
  let two_line = List.find_pred (function
                     | `Divider _ -> false
                     | `Item x    -> Option.is_some x#secondary_text)
                   items
                 |> Option.is_some
  in
  let elt = Markup.create ?avatar ~two_line
              ~items:(List.map (function
                          | `Divider x -> Widget.to_markup x
                          | `Item x    -> Widget.to_markup x)
                        items) ()
            |> Tyxml_js.To_dom.of_element in
  object(self)
    inherit Widget.t elt () as super

    method set_dense x    = self#add_or_remove_class x Markup.dense_class

    method add_item (x : Item.t) = Dom.appendChild self#root x#root
    method remove_item (x : Item.t) = try Dom.removeChild self#root x#root with _ -> ()
  end

module List_group = struct

  type group =
    { subheader : Typography.Text.t option
    ; list      : t
    }

  let rec add_dividers acc l =
    match l with
    | []       -> acc
    | hd :: [] -> List.rev @@ hd :: acc
    | hd :: tl -> add_dividers ((hd @ [Widget.to_markup @@ new Divider.t ()]) :: acc) tl

  class t ?(dividers=true) ~(content:group list) () =

    let elt = Markup.List_group.create
                ~content:(List.map (fun x -> let h = Option.map Widget.to_markup x.subheader in
                                             [Widget.to_markup x.list]
                                             |> List.cons_maybe h)
                            content
                          |> (fun x -> if dividers then add_dividers [] x else x)
                          |> List.flatten)
                ()
              |> Tyxml_js.To_dom.of_div in

    object
      inherit Widget.t elt ()

      method content = content
    end

end

