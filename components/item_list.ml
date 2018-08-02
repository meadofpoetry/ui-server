open Containers
open Tyxml_js

module Markup = Components_markup.Item_list.Make(Xml)(Svg)(Html)

type selection =
  [ `Single
  | `Multiple ]

module Item = struct

  class ['a] t ?(ripple=false)
          ?secondary_text
          ?graphic
          ?meta
          ?tag
          ~(value:'a)
          ~text () =
    let text_elt, set_primary, set_secondary = match secondary_text with
      | Some st ->
         let primary   = Markup.Item.create_primary_text text ()
                         |> Tyxml_js.To_dom.of_element
                         |> Widget.create in
         let secondary = Markup.Item.create_secondary_text st ()
                         |> Tyxml_js.To_dom.of_element
                         |> Widget.create in
         let w = Markup.Item.create_text
                   ~primary:(Widget.to_markup primary)
                   ~secondary:(Widget.to_markup secondary) ()
                 |> Tyxml_js.To_dom.of_element
                 |> Widget.create in
         let set_primary x   = primary#set_text_content x in
         let set_secondary x = secondary#set_text_content x in
         w, set_primary, set_secondary
      | None ->
         let primary = Markup.Item.create_text_simple text ()
                       |> Tyxml_js.To_dom.of_element
                       |> Widget.create in
         let set_primary x   = primary#set_text_content x in
         let set_secondary x = failwith "single-line list!" in
         primary, set_primary, set_secondary
    in
    let elt = Markup.Item.create
                ?graphic:(Option.map Widget.to_markup graphic)
                ?meta:(Option.map Widget.to_markup meta)
                ?tag (Widget.to_markup text_elt) ()
              |> Tyxml_js.To_dom.of_element in

    object(self)
      val mutable _v = value
      inherit Widget.t elt ()

      method set_text (x:string) = set_primary x
      method set_secondary_text (x:string) =
        set_secondary x

      (* TODO add setters, real getters *)
      method text           = text
      method secondary_text = secondary_text

      method value = _v
      method set_value (v:'a) = _v <- v

      method activated : bool =
        self#has_class Markup.Item.activated_class

      method selected : bool =
        self#has_class Markup.Item.selected_class

      initializer
        (* if ripple then Ripple.attach self |> ignore; *)
        Option.iter (fun x -> x#add_class Markup.Item.graphic_class) graphic;
        Option.iter (fun x -> x#add_class Markup.Item.meta_class) meta
    end

end

class base elt () =
object(self)
  inherit Widget.t elt ()

  method set_dense x = self#add_or_remove_class x Markup.dense_class

end

class ['a] t ?avatar
        ?(selection:selection option)
        ?two_line
        ?(dense=false)
        ~(items:[ `Item of 'a Item.t | `Divider of Divider.t ] list) () =
  let two_line = match two_line with
    | Some x -> x
    | None   ->
       List.find_pred (function
           | `Divider _ -> false
           | `Item x    -> Option.is_some x#secondary_text)
         items
       |> Option.is_some in
  let elt = Markup.create ?avatar ~two_line
              ~items:(List.map (function
                          | `Divider x -> Widget.to_markup x
                          | `Item x    -> Widget.to_markup x)
                        items) ()
            |> Tyxml_js.To_dom.of_element in
  let s_selected, set_selected = React.S.create [] in
  let s_active, set_active = React.S.create None in
  object(self)

    val mutable _items = items

    inherit base elt ()

    method items' = _items
    method items  = List.filter_map (function
                        | `Item i -> Some i
                        | _       -> None) _items

    method active : 'a Item.t option =
      React.S.value s_active
    method s_active : 'a Item.t option React.signal =
      s_active
    method set_active (item:'a Item.t) =
      List.iter (fun i ->
          if not @@ Equal.physical item i
          then i#remove_class Markup.Item.activated_class) self#items;
      item#add_class Markup.Item.activated_class;
      set_active (Some item)

    method selected : 'a Item.t list =
      React.S.value s_selected
    method s_selected : 'a Item.t list React.signal =
      s_selected
    method set_selected (item:'a Item.t) =
      match selection with
      | Some `Single ->
         List.iter (fun i ->
             if not @@ Equal.physical item i
             then i#remove_class Markup.Item.selected_class) self#items;
         item#add_class Markup.Item.selected_class;
         set_selected [item]
      | Some `Multiple ->
         item#add_class Markup.Item.selected_class;
         set_selected @@ item :: self#selected
      | None -> ()

    method dense : bool =
      self#has_class Markup.dense_class
    method set_dense (x:bool) : unit =
      self#add_or_remove_class x Markup.dense_class

    method append_item (x : 'a Item.t) =
      _items <- _items @ [ `Item x];
      Dom.appendChild self#root x#root

    method cons_item (x : 'a Item.t) =
      _items <- (`Item x) :: _items;
      Dom.insertBefore self#root x#root self#node##.firstChild

    method insert_item_at_idx (index:int) (x : 'a Item.t) =
      _items <- List.insert_at_idx index (`Item x) _items;
      let child = self#root##.childNodes##item index in
      Dom.insertBefore self#root x#root child

    method remove_item (x : 'a Item.t) =
      match List.find_idx (function
                | `Item i -> Equal.physical i x
                | _       -> false) self#items' with
      | Some (i, (`Item x)) ->
         Dom.removeChild self#root x#root;
         _items <- List.remove_at_idx i _items
      | Some _ | None  -> ()

    initializer
      self#set_dense dense
  end

module List_group = struct

  type group =
    { subheader : Typography.Text.t option
    ; list      : base
    }

  let rec add_dividers acc l =
    match l with
    | []       -> acc
    | hd :: [] -> List.rev @@ hd :: acc
    | hd :: tl ->
       add_dividers ((hd @ [Widget.to_markup @@ new Divider.t ()])
                     :: acc) tl

  class t ?(dividers=true) ~(content:group list) () =

    let elt =
      Markup.List_group.create
        ~content:(
          List.map (fun x ->
              let h = Option.map (fun w ->
                          w#add_class Markup.List_group.subheader_class;
                          Widget.to_markup w) x.subheader in
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

