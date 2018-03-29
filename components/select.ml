open Containers

module Base = struct

  class type mdc =
    object
      method value           : Js.js_string Js.t Js.readonly_prop
      method options         : Dom_html.element Js.js_array Js.t Js.readonly_prop
      method selectedIndex   : int Js.prop
      method selectedOptions : Dom_html.element Js.js_array Js.t Js.readonly_prop
      method disabled        : bool Js.t Js.prop
      method item            : (Js.number Js.t -> Dom_html.element Js.t Js.opt) Js.meth
      method nameditem       : (Js.js_string Js.t -> Dom_html.element Js.t Js.opt) Js.meth
    end

  class type change_event =
    object
      inherit Dom_html.event
      method detail_ : mdc Js.t Js.readonly_prop
    end

  type events =
    { change : change_event Js.t Dom_events.Typ.typ
    }

  let events =
    { change = Dom_events.Typ.make "MDCSelect:change"
    }

  module Item = struct

    class ['a] t ?id ?start_detail ?end_detail ~text ~(value:'a) () = object

      inherit Menu.Item.t ?start_detail ?end_detail ~text () as super

      val mutable value : 'a = value

      method get_value   = value
      method set_value x = value <- x

      initializer
        Option.iter (fun x -> super#set_id x) id;
        super#set_attribute "role" "option"

    end

  end

  class ['a] t ~label ~(items:'a Item.t list) () =

    let s_selected,s_selected_push = React.S.create None in
    let menu = new Menu.t ~items:(List.map (fun x -> `Item (x : 'a Item.t :> Menu.Item.t)) items) () in
    let () = menu#add_class Markup.Select.Base.menu_class in
    let elt = Markup.Select.Base.create ~label ~menu:(Widget.widget_to_markup menu) ()
              |> Tyxml_js.To_dom.of_element in
    let label_widget = elt##querySelector (Js.string @@ "." ^ Markup.Select.Base.label_class)
                       |> Js.Opt.to_option
                       |> Option.get_exn
                       |> (fun x -> new Widget.widget x ()) in

    object(self)

      inherit Widget.widget elt ()

      val mdc : mdc Js.t = elt |> (fun x -> Js.Unsafe.global##.mdc##.select##.MDCSelect##attachTo x)
      val items = items

      method s_selected  =  s_selected

      method get_menu    = menu
      method set_dense x = menu#get_list#set_dense x

      method get_items : 'a Item.t list = items
      method get_length  = List.length self#get_items
      method get_item n  = List.get_at_idx n self#get_items
      method get_named_item key = List.find_pred (fun x -> String.equal x#get_id key && (match x#get_attribute "name" with
                                                                                         | Some n -> String.equal n key
                                                                                         | None   -> false))
                                    self#get_items

      (* Selected getters *)

      method get_selected_value : 'a option =
        Option.map (fun x -> x#get_value) self#get_selected_item
      method get_selected_index : int option =
        mdc##.selectedIndex |> (fun x -> if x = -1 then None else Some x)
      method get_selected_item : 'a Item.t option =
        Option.map (fun x -> Option.get_exn @@ List.get_at_idx x self#get_items) self#get_selected_index

      (* Selected setters *)

      method select_value v = match List.find_idx (fun x -> Equal.poly x#get_value v) self#get_items with
        | Some (idx,_) -> Ok (self#select_at_index idx)
        | None         -> Error "Select.Base,select value: no item found with provided value"
      method select_at_index i = mdc##.selectedIndex := i;
                                 s_selected_push self#get_selected_value;
                                 label_widget#add_class Markup.Select.Base.label_float_above_class
      method select_item i     = (match List.find_idx (fun x -> Equal.physical x i) self#get_items with
                                  | Some (idx,_) -> self#select_at_index idx
                                  | None         -> ())

      method get_disabled   = Js.to_bool mdc##.disabled
      method set_disabled x = mdc##.disabled := Js.bool x

      method layout = let width =
                        List.fold_left (fun acc x -> Printf.printf "item width is %d\n"
                                                       x#get_offset_width;
                                                     if x#get_offset_width > acc
                                                     then x#get_offset_width
                                                     else acc) 0 self#get_items
                      in
                      self#root##.style##.width := Js.string @@ (string_of_int width)^"px"

      initializer
        self#set_on_load @@ Some (fun () -> self#layout);
        (* FIXME add open listener, - opens not only by click *)
        Dom_events.listen self#root
          Dom_events.Typ.click
          (fun _ _ -> let w = Printf.sprintf "%dpx" self#get_offset_width in
                      self#get_menu#style##.width := Js.string w;
                      true)
        |> ignore;
        Dom_events.listen self#root
          events.change
          (fun _ _ -> s_selected_push @@ self#get_selected_value; false)
        |> ignore

    end

end

module Pure = struct

  module Item = struct

    class t ?disabled ?value ~text () =

      let elt = Markup.Select.Pure.Item.create ?disabled ?value ~text () |> Tyxml_js.To_dom.of_option in

      object(self)

        inherit Widget.widget elt ()

        method option_element = elt

        method get_text    = Js.to_string self#option_element##.text
        method get_value   = Js.to_string self#option_element##.value
        method set_value v = self#option_element##.value := Js.string v
        method get_index   = self#option_element##.index

        method is_selected    = Js.to_bool self#option_element##.selected
        method set_selected x = self#option_element##.selected := Js.bool x

        method get_disabled   = Js.to_bool self#option_element##.disabled
        method set_disabled x = self#option_element##.disabled := Js.bool x

      end

  end

  module Group = struct

    class t ~label ~(items:Item.t list) () =

      let item_elts = List.map (fun x -> Tyxml_js.Of_dom.of_option x#option_element) items in
      let elt = Markup.Select.Pure.Item.create_group ~label ~items:item_elts () |> Tyxml_js.To_dom.of_optgroup in

      object(self)

        inherit Widget.widget elt ()

        method opt_group_element = elt

        method get_items   = items
        method get_label   = Js.to_string self#opt_group_element##.label
        method set_label s = self#opt_group_element##.label := Js.string s

        method get_disabled   = Js.to_bool self#opt_group_element##.disabled
        method set_disabled x = self#opt_group_element##.disabled := Js.bool x

      end

  end

  class t ~(items : [ `Item of Item.t | `Group of Group.t ] list) () =

    let item_elts = List.map (function
                        | `Group g -> Widget.widget_to_markup g
                        | `Item i  -> Widget.widget_to_markup i) items in
    let elt = Markup.Select.Pure.create ~items:item_elts () |> Tyxml_js.To_dom.of_div in
    let select_elt = elt##querySelector (Js.string ("." ^ Markup.Select.surface_class))
                     |> Js.Opt.to_option |> Option.get_exn |> Js.Unsafe.coerce in

    object(self)

      inherit Widget.widget elt ()

      method select_element : Dom_html.selectElement Js.t = select_elt

      method get_value = Js.to_string self#select_element##.value
      method get_items = List.fold_left (fun acc x -> match x with
                                                      | `Group g -> acc @ g#get_items
                                                      | `Item i  -> acc @ [i]) [] items

      method get_length = self#select_element##.length
      method get_item n = List.get_at_idx n self#get_items

      method get_selected_index = self#select_element##.selectedIndex |> (fun x -> if x = -1 then None else Some x)
      method get_selected_item  = Option.map (fun x -> List.get_at_idx x self#get_items) self#get_selected_index
      method select_at_index i  = self#select_element##.selectedIndex := i

      method get_disabled   = Js.to_bool self#select_element##.disabled
      method set_disabled x = self#select_element##.disabled := Js.bool x

    end

end

module Multi = struct

  module Divider = struct
    class t () = object
      inherit Widget.widget (Markup.Select.Multi.Item.create_divider () |> Tyxml_js.To_dom.of_option) ()
    end
  end

  module Item = struct
    class t ?disabled ?value ~text () = object
      inherit Pure.Item.t ?disabled ?value ~text () as super
      initializer
        super#add_class Markup.List_.Item._class
    end
  end

  module Group = struct
    class t ~label ~(items:Item.t list) () = object
      inherit Pure.Group.t ~label ~items:(List.map (fun x -> (x : Item.t :> Pure.Item.t)) items) () as super
      initializer
        super#add_class Markup.List_.List_group._class
    end
  end

  class t ?size ~(items:[ `Item of Item.t | `Divider of Divider.t | `Group of Group.t ] list) () =

    let item_elts = List.map (function
                        | `Divider d -> Widget.widget_to_markup d
                        | `Group g   -> Widget.widget_to_markup g
                        | `Item i    -> i#style##.paddingLeft := Js.string "32px"; Widget.widget_to_markup i)
                      items in
    let elt = Markup.Select.Multi.create ?size ~items:item_elts () |> Tyxml_js.To_dom.of_select in

    object(self)

      inherit Widget.widget elt ()

      method select_element = elt

      method get_items = List.fold_left (fun acc x -> match x with
                                                      | `Divider _ -> acc
                                                      | `Group g   -> acc @ g#get_items
                                                      | `Item i    -> acc @ [i]) [] items

      method get_length = self#select_element##.length
      method get_item n = List.get_at_idx n self#get_items

      method get_selected_index = self#select_element##.selectedIndex |> (fun x -> if x = -1 then None else Some x)
      method get_selected_item  = Option.map (fun x -> List.get_at_idx x self#get_items) self#get_selected_index
      method select_at_index i  = self#select_element##.selectedIndex := i

      method get_disabled   = Js.to_bool self#select_element##.disabled
      method set_disabled x = self#select_element##.disabled := Js.bool x

    end

end
