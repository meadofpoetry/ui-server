module Item = struct

  class ['a] t ?ripple
             ?secondary_text
             ?(start_detail:#Widget.widget option)
             ?(end_detail:#Widget.widget option)
             ?(nested:'a option)
             ~text
             () =

    let s,s_push = React.S.create false in
    let end_detail =
      (match end_detail with
       | Some x -> Some x
       | None   -> CCOpt.map (fun _ -> let icon = new Icon.Font.t ~icon:"expand_more" () in
                                       React.S.map (fun x -> if x then icon#set_icon "expand_less"
                                                             else icon#set_icon "expand_more") s |> ignore;
                                       icon)
                             nested) in

    let item = new Item_list.Item.t ?ripple ?secondary_text ?start_detail ?end_detail ~text () in

    let elt = Markup.Tree.Item.create ~item:(Widget.widget_to_markup item)
                                      ?nested_list:(CCOpt.map (fun x -> Widget.widget_to_markup x) nested)
                                      ()
              |> Tyxml_js.To_dom.of_element in

    object

      inherit Widget.widget elt () as super

      method get_item           = item
      method get_text           = item#get_text
      method get_secondary_text = item#get_secondary_text
      method get_nested_tree : 'a option = nested

      initializer
        CCOpt.iter (fun x -> x#add_class Markup.Tree.Item.nested_list_class;
                             x#add_class Markup.Tree.Item.nested_list_hidden_class;
                             item#style##.cursor := Js.string "pointer") nested;
        Dom_events.listen super#root
                          Dom_events.Typ.click
                          (fun _ e -> let hidden_class = Markup.Tree.Item.nested_list_hidden_class in
                                      Dom_html.stopPropagation e;
                                      CCOpt.iter (fun x -> x#toggle_class hidden_class |> not |> s_push) nested;
                                      true)
        |> ignore;
    end

end

class t ~(items:t Item.t list) () =

  let two_line = CCOpt.is_some @@ CCList.find_pred (fun x -> CCOpt.is_some x#get_secondary_text) items in
  let elt = Markup.Tree.create ~two_line ~items:(Widget.widgets_to_markup items) ()
            |> Tyxml_js.To_dom.of_element in

  object(self)

    val mutable items = items

    inherit Widget.widget elt () as super

    method get_items   = items
    method set_dense x = if x
                         then (super#add_class Markup.List_.dense_class;
                               self#iter (fun (i:t Item.t) -> CCOpt.iter (fun (t:t) -> t#set_dense x)
                                                                         i#get_nested_tree))
                         else (super#remove_class Markup.List_.dense_class;
                               self#iter (fun (i:t Item.t) -> CCOpt.iter (fun (t:t) -> t#set_dense x)
                                                                         i#get_nested_tree))

    method private iter f = let rec iter l = CCList.iter (fun (x : t Item.t) -> f x;
                                                                                match x#get_nested_tree with
                                                                                | Some n -> iter n#get_items
                                                                                | None   -> ()) l in
                            iter self#get_items

  end
