open Containers
open Tyxml_js

module Markup = Components_markup.Tree.Make(Xml)(Svg)(Html)

module Item = struct

  class ['a] t ?ripple
          ?secondary_text
          ?(graphic:#Widget.widget option)
          ?(meta:#Widget.widget option)
          ?(nested:'a option)
          ~text
          () =

    let s,s_push = React.S.create false in
    let meta =
      (match meta with
       | Some x -> Some x
       | None   -> Option.map (fun _ -> let icon = new Icon.Font.t ~icon:"expand_more" () in
                                        React.S.map (fun x -> if x then icon#set_icon "expand_less"
                                                              else icon#set_icon "expand_more") s |> ignore;
                                        icon)
                     nested) in

    let item = new Item_list.Item.t ?ripple ?secondary_text ?graphic ?meta ~text () in

    let elt = Markup.Item.create ~item:(Widget.widget_to_markup item)
                ?nested_list:(Option.map (fun x -> Widget.widget_to_markup x) nested)
                ()
              |> Tyxml_js.To_dom.of_element in

    object

      inherit Widget.widget elt () as super

      method item           = item
      method text           = item#text
      method secondary_text = item#secondary_text
      method nested_tree : 'a option = nested

      initializer
        Option.iter (fun x -> x#add_class Markup.Item.list_class;
                              item#style##.cursor := Js.string "pointer") nested;
        Dom_events.listen super#root
          Dom_events.Typ.click
          (fun _ e -> let open_class = Markup.Item.item_open_class in
                      let open_list  = Markup.Item.list_open_class in
                      Dom_html.stopPropagation e;
                      let list = (Js.Unsafe.coerce super#root)##querySelector (Js.string ".mdc-tree__list") in
                      let _ = list##.classList##toggle open_list in
                      super#toggle_class open_class |> s_push;
                      true)
        |> ignore;
    end

end

class t ~(items:t Item.t list) () =

  let two_line = Option.is_some @@ List.find_pred (fun x -> Option.is_some x#secondary_text) items in
  let elt      = Markup.create ~two_line ~items:(Widget.widgets_to_markup items) ()
                 |> Tyxml_js.To_dom.of_element in

  object(self)

    val mutable items = items

    inherit Widget.widget elt () as super

    method items = items

    method set_dense x =
      self#add_or_remove_class x Markup.dense_class;
      self#iter (fun (i:t Item.t) -> Option.iter (fun (t:t) -> t#set_dense x)
                                       i#nested_tree)

    method private iter f =
      let rec iter l = List.iter (fun (x : t Item.t) ->
                           f x;
                           match x#nested_tree with
                           | Some n -> iter n#items
                           | None   -> ()) l in
      iter self#items

    method private padding () =
      let rec iter l n = List.iter (fun x ->
                             let item = (Js.Unsafe.coerce x#root)##querySelector (Js.string ".mdc-list-item") in
                             item##.style##.paddingLeft := Js.string @@ (string_of_int (n*16))^"px";
                             match x#nested_tree with
                             | Some el -> iter el#items (n+1)
                             | None   -> ()) l in
      iter self#items 1

    initializer
      self#padding ();

  end
