open Containers
open Tyxml_js

module Markup = Components_markup.Tree.Make(Xml)(Svg)(Html)

module Item = struct

  class ['a,'b] t ?ripple
          ?secondary_text
          ?(graphic:#Widget.t option)
          ?(meta:#Widget.t option)
          ?(nested:'b option)
          ~(value:'a)
          ~text
          () =

    let s,s_push = React.S.create false in
    let meta =
      (match meta with
       | Some x -> Some x
       | None   ->
          Option.map (fun _ ->
              let icon = new Icon.SVG.t ~icon:Chevron_down () in
              React.S.map (fun x ->
                  if x then icon#set_icon Chevron_up
                  else icon#set_icon Chevron_down) s |> ignore;
              icon#widget)
            nested) in
    let item = new Item_list.Item.t ?ripple ?secondary_text
                 ?graphic ?meta ~tag:Html.div ~value ~text () in
    let elt  = Markup.Item.create ~item:(Widget.to_markup item)
                 ?nested_list:(Option.map Widget.to_markup nested) ()
               |> Tyxml_js.To_dom.of_element in
    object(self)

      inherit Widget.t elt ()

      method item = item
      method nested_tree : 'b option = nested

      initializer
        Option.iter (fun x ->
            x#add_class Markup.Item.list_class;
            item#style##.cursor := Js.string "pointer") nested;
        Dom_events.listen self#root Dom_events.Typ.click (fun _ e ->
            let open_class = Markup.Item.item_open_class in
            let open_list  = Markup.Item.list_open_class in
            Dom_html.stopPropagation e;
            Option.iter (fun x -> x#toggle_class open_list |> ignore)
              self#nested_tree;
            self#toggle_class open_class |> s_push;
            true)
        |> ignore;
    end

end

class ['a] t ~(items:('a,'a t) Item.t list) () =
  let two_line =
    List.find_pred (fun x -> Option.is_some x#item#secondary_text) items
    |> Option.is_some in
  let elt = Markup.create ~two_line
              ~items:(List.map Widget.to_markup items) ()
            |> Tyxml_js.To_dom.of_element in
  object(self)

    val mutable items = items

    inherit Widget.t elt () as super

    method items = items

    method set_dense x =
      self#add_or_remove_class x Markup.dense_class;
      self#iter (fun (i:('a,'a t) Item.t) ->
          Option.iter (fun (t:'a t) -> t#set_dense x)
            i#nested_tree)

    method private iter f =
      let rec iter l = List.iter (fun (x : ('a,'a t) Item.t) ->
                           f x;
                           match x#nested_tree with
                           | Some n -> iter n#items
                           | None   -> ()) l in
      iter self#items

    method private _padding () =
      let rec iter l n =
        List.iter (fun x ->
            let item = (Js.Unsafe.coerce x#root)##querySelector (Js.string ".mdc-list-item") in
            item##.style##.paddingLeft := Js.string @@ (string_of_int (n*16))^"px";
            match x#nested_tree with
            | Some el -> iter el#items (n+1)
            | None   -> ()) l in
      iter self#items 1

    initializer
      self#_padding ();

  end
