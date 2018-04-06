open Containers

module Divider = struct
  class t ?inset () = object
    inherit Widget.widget (Markup.List_.Item.create_divider ?inset () |> Tyxml_js.To_dom.of_element) ()
  end
end

module Item = struct

  (* TODO add ripple manually, without auto-init *)
  class t ?(ripple=false) ?secondary_text ?start_detail ?end_detail ~text () =

    let elt = Markup.List_.Item.create ?secondary_text
                ?start_detail:(Option.map Widget.widget_to_markup start_detail)
                ?end_detail:(Option.map Widget.widget_to_markup end_detail)
                ~text ()
              |> Tyxml_js.To_dom.of_element in

    object(self)
      inherit Widget.widget elt ()

      (* TODO add setters, real getters *)
      method get_text           = text
      method get_secondary_text = secondary_text

      initializer
        if ripple then Ripple.attach self |> ignore;
        Option.iter (fun x -> x#add_class Markup.List_.Item.graphic_class) start_detail;
        Option.iter (fun x -> x#add_class Markup.List_.Item.meta_class) end_detail
    end

end

class t ?avatar ~(items:[ `Item of Item.t | `Divider of Divider.t ] list) () =

  let two_line = Option.is_some @@ List.find_pred (function
                                       | `Divider _ -> false
                                       | `Item x    -> Option.is_some x#get_secondary_text)
                                     items in

  let elt = Markup.List_.create ?avatar ~two_line ~items:(List.map (function
                                                              | `Divider x -> Widget.widget_to_markup x
                                                              | `Item x    -> Widget.widget_to_markup x)
                                                            items) ()
            |> Tyxml_js.To_dom.of_element in

  object(self)

    inherit Widget.widget elt () as super

    method private add_or_rm_class x c = if x then super#add_class c else super#remove_class c
    method set_dense x    = self#add_or_rm_class x Markup.List_.dense_class
    method set_bordered x = self#add_or_rm_class x Markup.List_.bordered_class

    method add_item (x : Item.t) = Dom.appendChild self#root x#root
    method remove_item (x : Item.t) = try Dom.removeChild self#root x#root with _ -> ()
(*
    method get_items : Item.t list =
      List.filter_map (function `Item x -> Some x | `Divider _ -> None) items 
 *)
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
    | hd :: tl -> add_dividers ((hd @ [Widget.widget_to_markup @@ new Divider.t ()]) :: acc) tl

  class t ?(dividers=true) ~(content:group list) () =

    let elt = Markup.List_.List_group.create
                ~content:(List.map (fun x -> let h = Option.map Widget.widget_to_markup x.subheader in
                                             [Widget.widget_to_markup x.list]
                                             |> List.cons_maybe h)
                            content
                          |> (fun x -> if dividers then add_dividers [] x else x)
                          |> List.flatten)
                ()
              |> Tyxml_js.To_dom.of_div in

    object
      inherit Widget.widget elt ()

      method content = content
    end

end

