module Divider = struct
  class t ?inset () = object
    inherit Widget.widget (Markup.List_.Item.create_divider ?inset () |> Tyxml_js.To_dom.of_element) ()
  end
end

module Item = struct

  (* TODO add ripple manually, without auto-init *)
  class t ?(ripple=false) ?secondary_text ?start_detail ?end_detail ~text () =

    let elt = Markup.List_.Item.create ?secondary_text
                                       ?start_detail:(CCOpt.map Widget.widget_to_markup start_detail)
                                       ?end_detail:(CCOpt.map Widget.widget_to_markup end_detail)
                                       ~text ()
              |> Tyxml_js.To_dom.of_element in

    object(self)
      inherit Widget.widget elt ()

      (* TODO add setters, real getters *)
      method get_text           = text
      method get_secondary_text = secondary_text

      initializer
        if ripple then Ripple.attach self |> ignore;
        CCOpt.iter (fun x -> x#add_class Markup.List_.Item.start_detail_class) start_detail;
        CCOpt.iter (fun x -> x#add_class Markup.List_.Item.end_detail_class) end_detail
    end

end

class t ?avatar ~(items:[ `Item of Item.t | `Divider of Divider.t ] list) () =

  let two_line = CCOpt.is_some @@ CCList.find_pred (function
                                                    | `Divider _ -> false
                                                    | `Item x    -> CCOpt.is_some x#get_secondary_text)
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

    method get_items : Item.t list =
      CCList.filter_map (function `Item x -> Some x | `Divider _ -> None) items

  end

module List_group = struct

  type group =
    { subheader : string option
    ; list      : t
    }

  module Subheader = struct
    class t ~text () = object(self)
      inherit Widget.widget (Markup.List_.List_group.create_subheader ~text () |> Tyxml_js.To_dom.of_element) ()
      method get_text   = self#get_text_content |> CCOpt.get_or ~default:""
      method set_text s = self#set_text_content s
    end
  end

  let rec add_dividers acc l =
    match l with
    | []       -> acc
    | hd :: [] -> List.rev @@ hd :: acc
    | hd :: tl -> add_dividers ((hd @ [Widget.widget_to_markup @@ new Divider.t ()]) :: acc) tl

  class t ?(dividers=true) ~(content:group list) () =

    let content = List.map (fun gp -> (CCOpt.map (fun x -> new Subheader.t ~text:x ()) gp.subheader, gp.list))
                           content in

    let elt = Markup.List_.List_group.create
                ~content:(CCList.map (fun (h,l) -> let h = CCOpt.map Widget.widget_to_markup h in
                                                   [Widget.widget_to_markup l]
                                                   |> CCList.cons_maybe h)
                                     content
                          |> (fun x -> if dividers then add_dividers [] x else x)
                          |> CCList.flatten)
                ()
              |> Tyxml_js.To_dom.of_div in

    object
      inherit Widget.widget elt ()

      method get_lists      = CCList.map (fun (_,l) -> l) content
      method get_subheaders = CCList.map (fun (h,_) -> h) content
    end

end

