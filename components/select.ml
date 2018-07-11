open Containers
open Tyxml_js

module Markup = Components_markup.Select.Make(Xml)(Svg)(Html)

module Item = struct
  class ['a] t ?selected ?disabled ~(value:'a) ~text () =
    let elt = Markup.Item.create ?disabled ~text () |> Tyxml_js.To_dom.of_option in
    object(self)

      val mutable _value = value

      inherit Widget.t elt ()

      method value            = _value
      method set_value (x:'a) = _value <- x

      method text        = Js.to_string self#option_element##.text
      method index       = self#option_element##.index

      method selected       = Js.to_bool self#option_element##.selected
      method set_selected x = self#option_element##.selected := Js.bool x

      method disabled       = Js.to_bool self#option_element##.disabled
      method set_disabled x = self#option_element##.disabled := Js.bool x

      method private option_element = elt

      initializer
        Option.iter (fun x -> self#set_selected x) selected
    end
end

module Group = struct
  class ['a] t ~label ~(items:'a Item.t list) () =

    let item_elts = List.map (fun x -> Tyxml_js.Of_dom.of_option (Js.Unsafe.coerce x#root)) items in
    let elt = Markup.Item.create_group ~label ~items:item_elts ()
              |> Tyxml_js.To_dom.of_optgroup in

    object(self)

      inherit Widget.t elt ()

      method opt_group_element = elt

      method items       = items
      method label       = Js.to_string self#opt_group_element##.label
      method set_label s = self#opt_group_element##.label := Js.string s

      method disabled       = Js.to_bool self#opt_group_element##.disabled
      method set_disabled x = self#opt_group_element##.disabled := Js.bool x

    end
end

module Label = struct
  class t ~s_selected ~label () =
    let elt = Markup.Label.create ~label () |> Tyxml_js.To_dom.of_element in
    object(self)
      inherit Widget.t elt ()
      inherit Widget.stateful ()
      initializer
        React.S.map (fun v -> self#add_or_remove_class (Option.is_some v) Markup.Label.float_above_class)
          s_selected
        |> self#_keep_s
    end
end

module Bottom_line = struct
  class t () =
    let elt = Markup.Bottom_line.create () |> Tyxml_js.To_dom.of_element in
    object(self)
      inherit Widget.t elt ()

      method activate (x:bool) = self#add_or_remove_class x Markup.Bottom_line.active_class
    end
end

class ['a] t ?(disabled=false)
        ?(default_selected=true)
        ~label
        ~(items:[ `Item of 'a Item.t | `Group of 'a Group.t ] list)
        () =
  let make_empty () = Markup.Item.create ~disabled:true ~selected:true ~text:"" () in
  let s,push        = React.S.create None in
  let s_value       = React.S.map (fun i -> Option.map (fun x -> x#value) i) s in
  let item_elts     = List.map (function `Group g -> Widget.to_markup g
                                       | `Item i  -> Widget.to_markup i) items
                      |> (fun l -> if not default_selected then make_empty () :: l else l)
  in
  let bottom_line = new Bottom_line.t () in
  let label       = new Label.t ~label ~s_selected:s_value () in
  let select      = Markup.create_select ~items:item_elts ()
                    |> Tyxml_js.To_dom.of_element
                    |> Widget.create
  in
  let elt = Markup.create ~bottom_line:(Widget.to_markup bottom_line)
              ~label:(Widget.to_markup label)
              ~select:(Widget.to_markup select)
              ()
            |> Tyxml_js.To_dom.of_div
  in
  object(self)
    val mutable _items = items
    inherit Widget.t elt ()

    method select      = select
    method bottom_line = bottom_line
    method label       = label

    method value : 'a option =
      Option.map (fun (x:'a Item.t) -> x#value) self#selected_item
    method items : 'a Item.t list =
      List.fold_left (fun acc x -> match x with
                                   | `Group g -> g#items @ acc
                                   | `Item i  -> i::acc) [] _items

    method length : int =
      self#_native_select##.length
    method item n : 'a Item.t option =
      List.get_at_idx n self#items

    method! set_empty () =
      self#select#set_empty (); push None;
      if not default_selected
      then Dom.appendChild self#select#root
           @@ Tyxml_js.To_dom.of_option @@ make_empty ()

    method append_item (i:'a Item.t) =
      _items <- `Item i :: _items;
      Dom.appendChild self#select#root i#root
    method append_group (g:'a Group.t) =
      _items <- `Group g :: _items;
      Dom.appendChild self#select#root g#root

    method selected_index : int option =
      self#_native_select##.selectedIndex |> (fun x -> if x = -1 then None else Some x)
    method selected_item : 'a Item.t option =
      Option.flat_map (fun x -> List.find_opt (fun i -> i#index = x) self#items) self#selected_index
    method set_selected_index i =
      self#_native_select##.selectedIndex := i;
      self#add_class Markup.is_changing_class;
      Dom_html.setTimeout (fun () -> self#remove_class Markup.is_changing_class) 125.0 |> ignore;
      push @@ List.find_opt (fun x -> x#index = i) self#items
    method set_selected_value ~(eq:'a -> 'a -> bool) (v:'a) =
      match List.find_opt (fun (x:'a Item.t) -> eq x#value v) self#items with
      | Some i -> self#set_selected_index i#index; Ok i
      | None   -> Error "item not found"
    method s_selected_item  : 'a Item.t option React.signal = s
    method s_selected_value : 'a option React.signal = s_value

    method disabled : bool =
      Js.to_bool self#_native_select##.disabled
    method set_disabled (x:bool) : unit =
      self#add_or_remove_class x Markup.disabled_class;
      self#_native_select##.disabled := Js.bool x

    method private _native_select : Dom_html.selectElement Js.t = Js.Unsafe.coerce select#root

    initializer
      push self#selected_item;
      Dom_events.(listen select#root Typ.focus (fun _ _ -> self#bottom_line#activate true;true))
      |> ignore;
      Dom_events.(listen select#root Typ.blur (fun _ _ -> self#bottom_line#activate false;true))
      |> ignore;
      Dom_events.(listen select#root Typ.change (fun _ _ ->
                      Option.iter self#set_selected_index self#selected_index; true))
      |> ignore;
      self#set_disabled disabled

  end


