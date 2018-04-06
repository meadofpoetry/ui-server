open Containers

module Item = struct

  class ['a] t ?disabled ~(value:'a) ~text () =

    let elt = Markup.Select.Item.create ?disabled ~text () |> Tyxml_js.To_dom.of_option in

    object(self)

      val mutable _value = value

      inherit Widget.widget elt ()

      method value            = _value
      method set_value (x:'a) = _value <- x

      method text        = Js.to_string self#option_element##.text
      method index       = self#option_element##.index

      method selected       = Js.to_bool self#option_element##.selected
      method set_selected x = self#option_element##.selected := Js.bool x

      method disabled       = Js.to_bool self#option_element##.disabled
      method set_disabled x = self#option_element##.disabled := Js.bool x

      method private option_element = elt

    end

end

module Group = struct

  class ['a] t ~label ~(items:'a Item.t list) () =

    let item_elts = List.map (fun x -> Tyxml_js.Of_dom.of_option (Js.Unsafe.coerce x#root)) items in
    let elt = Markup.Select.Item.create_group ~label ~items:item_elts ()
              |> Tyxml_js.To_dom.of_optgroup in

    object(self)

      inherit Widget.widget elt ()

      method opt_group_element = elt

      method items       = items
      method label       = Js.to_string self#opt_group_element##.label
      method set_label s = self#opt_group_element##.label := Js.string s

      method disabled       = Js.to_bool self#opt_group_element##.disabled
      method set_disabled x = self#opt_group_element##.disabled := Js.bool x

    end

end

module Bottom_line = struct

  let active_class = Markup.CSS.add_modifier Markup.Select.bottom_line_class "active"

  class t () =
    let elt = Markup.Select.create_bottom_line () |> Tyxml_js.To_dom.of_element in
    object(self)
      inherit Widget.widget elt ()

      method activate (x:bool) = self#add_or_remove_class x active_class
    end

end

class ['a] t ?(disabled=false)
        ~label
        ~(items : [ `Item of 'a Item.t | `Group of 'a Group.t ] list)
        () =

  let item_elts   = List.map (function `Group g -> Widget.widget_to_markup g
                                     | `Item i  -> Widget.widget_to_markup i) items in
  let bottom_line = new Bottom_line.t () in
  let label       = Markup.Select.create_label ~label ()
                    |> Tyxml_js.To_dom.of_element
                    |> Widget.create
  in
  let select      = Markup.Select.create_select ~items:item_elts ()
                    |> Tyxml_js.To_dom.of_element
                    |> Widget.create
  in
  let elt = Markup.Select.create ~bottom_line:(Widget.widget_to_markup bottom_line)
              ~label:(Widget.widget_to_markup label)
              ~select:(Widget.widget_to_markup select)
              ()
            |> Tyxml_js.To_dom.of_div
  in
  let s,push = React.S.create None in

  object(self)

    val _s_value = React.S.map (fun i -> Option.map (fun x -> x#value) i) s

    inherit Widget.widget elt ()

    method select      = select
    method bottom_line = bottom_line
    method label       = label

    method value : 'a option =
      Option.map (fun (x:'a Item.t) -> x#value) self#selected_item
    method items : 'a Item.t list =
      List.fold_left (fun acc x -> match x with
                                   | `Group g -> g#items @ acc
                                   | `Item i  -> i::acc) [] items

    method length : int   =
      self#_native_select##.length
    method item n : 'a Item.t option =
      List.get_at_idx n self#items

    method selected_index : int option =
      self#_native_select##.selectedIndex |> (fun x -> if x = -1 then None else Some x)
    method selected_item : 'a Item.t option =
      Option.map (fun x -> List.get_at_idx_exn x self#items) self#selected_index
    method set_selected_index i = self#_native_select##.selectedIndex := i;
                                  push @@ self#item i
    method set_selected_value ~(eq:'a -> 'a -> bool) (v:'a) =
      match List.find_opt (fun (x:'a Item.t) -> eq x#value v) self#items with
      | Some i -> self#set_selected_index i#index; Ok i
      | None   -> Error "item not found"
    method s_selected_item  : 'a Item.t option React.signal = s
    method s_selected_value : 'a option React.signal = _s_value

    method disabled : bool =
      Js.to_bool self#_native_select##.disabled
    method set_disabled (x:bool) : unit =
      self#_native_select##.disabled := Js.bool x

    method private _native_select : Dom_html.selectElement Js.t = Js.Unsafe.coerce select#root

    initializer
      Dom_events.(listen select#root Typ.focus (fun _ _ -> self#bottom_line#activate true;true))
      |> ignore;
      Dom_events.(listen select#root Typ.blur (fun _ _ -> self#bottom_line#activate false;true))
      |> ignore;
      self#set_disabled disabled

  end


