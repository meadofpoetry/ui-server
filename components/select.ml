open Widget
open Tyxml_js

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
    class t ?id ?start_detail ?end_detail ~text () = object
      inherit Menu.Item.t ?start_detail ?end_detail ~text () as super

      initializer
        CCOpt.iter (fun x -> super#set_id x) id;
        super#set_attribute "role" "option"
    end
  end

  class t ?placeholder ~(items:Item.t list) () =

    let menu = new Menu.t ~items:(List.map (fun x -> `Item (x : Item.t :> Menu.Item.t)) items) () in
    let () = menu#add_class Markup.Select.Base.menu_class in
    let elt = Markup.Select.Base.create ?selected_text:placeholder ~menu:(widget_to_markup menu) ()
              |> To_dom.of_element in

    object(self)

      inherit widget elt ()

      val mdc : mdc Js.t = elt |> (fun x -> Js.Unsafe.global##.mdc##.select##.MDCSelect##attachTo x)
      val items = items

      method menu            = menu
      method dense           = menu#list#dense
      method not_dense       = menu#list#not_dense

      method value           = Js.to_string mdc##.value (* id of item (if available) or text *)

      method items           = items
      method length          = CCList.length self#items
      method item n          = CCList.get_at_idx n self#items
      method named_item key  = CCList.find_pred (fun x -> x#id = key && (match x#get_attribute "name" with
                                                                         | Some n -> n = key
                                                                         | None   -> false))
                                                self#items

      method selected_index    = mdc##.selectedIndex |> (fun x -> if x = -1 then None else Some x)
      method selected_item     = CCOpt.map (fun x -> CCList.get_at_idx x self#items) self#selected_index
      method select_at_index i = mdc##.selectedIndex := i
      method select_item i     = (match CCList.find_idx (fun x -> x == i) self#items with
                                  | Some (idx,_) -> self#select_at_index idx
                                  | None         -> ())

      method disabled        = Js.to_bool mdc##.disabled
      method disable         = mdc##.disabled := Js._true
      method enable          = mdc##.disabled := Js._false
      method toggle_disabled = mdc##.disabled := Js.bool @@ not self#disabled

    end

end

module Pure = struct

  module Item = struct

    class t ?disabled ?value ~text () =

      let elt = Markup.Select.Pure.Item.create ?disabled ?value ~text () |> To_dom.of_option in

      object(self)

        inherit widget elt ()

        method option_element = elt

        method text        = Js.to_string self#option_element##.text
        method value       = Js.to_string self#option_element##.value
        method set_value v = self#option_element##.value := Js.string v
        method index       = self#option_element##.index

        method selected    = Js.to_bool self#option_element##.selected
        method select      = self#option_element##.selected := Js._true
        method deselect    = self#option_element##.selected := Js._false

        method disabled        = Js.to_bool self#option_element##.disabled
        method disable         = self#option_element##.disabled := Js._true
        method enable          = self#option_element##.disabled := Js._false
        method toggle_disabled = self#option_element##.disabled := Js.bool @@ not self#disabled

      end

  end

  module Group = struct

    class t ~label ~(items:Item.t list) () =

      let item_elts = List.map (fun x -> Of_dom.of_option x#option_element) items in
      let elt = Markup.Select.Pure.Item.create_group ~label ~items:item_elts () |> To_dom.of_optgroup in

      object(self)

        inherit widget elt ()

        method opt_group_element = elt

        method items           = items
        method label           = Js.to_string self#opt_group_element##.label
        method set_label s     = self#opt_group_element##.label := Js.string s

        method disabled        = Js.to_bool self#opt_group_element##.disabled
        method disable         = self#opt_group_element##.disabled := Js._true
        method enable          = self#opt_group_element##.disabled := Js._false
        method toggle_disabled = self#opt_group_element##.disabled := Js.bool @@ not self#disabled

      end

  end

  class t ~(items : [ `Item of Item.t | `Group of Group.t ] list) () =

    let item_elts = List.map (function
                              | `Group g -> widget_to_markup g
                              | `Item i  -> widget_to_markup i) items in
    let elt = Markup.Select.Pure.create ~items:item_elts () |> To_dom.of_select in

    object(self)

      inherit widget elt ()

      method select_element = elt

      method value = Js.to_string self#select_element##.value

      method items = CCList.fold_left (fun acc x -> match x with
                                                    | `Group g -> acc @ g#items
                                                    | `Item i  -> acc @ [i])
                                      [] items

      method length = self#select_element##.length
      method item n = CCList.get_at_idx n self#items

      method selected_index    = self#select_element##.selectedIndex |> (fun x -> if x = -1 then None else Some x)
      method select_at_index i = self#select_element##.selectedIndex := i
      method selected_item     = CCOpt.map (fun x -> CCList.get_at_idx x self#items) self#selected_index

      method disabled        = Js.to_bool self#select_element##.disabled
      method disable         = self#select_element##.disabled := Js._true
      method enable          = self#select_element##.disabled := Js._false
      method toggle_disabled = self#select_element##.disabled := Js.bool @@ not self#disabled

    end

end

module Multi = struct

  module Divider = struct
    class t () = object
      inherit widget (Markup.Select.Multi.Item.create_divider () |> To_dom.of_option) ()
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
                              | `Divider d -> widget_to_markup d
                              | `Group g   -> widget_to_markup g
                              | `Item i    -> i#style##.paddingLeft := Js.string "32px";
                                              widget_to_markup i)
                             items in
    let elt = Markup.Select.Multi.create ?size ~items:item_elts () |> To_dom.of_select in

    object(self)

      inherit widget elt ()

      method select_element = elt

      method items = CCList.fold_left (fun acc x -> match x with
                                                    | `Divider _ -> acc
                                                    | `Group g   -> acc @ g#items
                                                    | `Item i    -> acc @ [i])
                                      [] items

      method length = self#select_element##.length
      method item n = CCList.get_at_idx n self#items

      method selected_index    = self#select_element##.selectedIndex |> (fun x -> if x = -1 then None else Some x)
      method select_at_index i = self#select_element##.selectedIndex := i
      method selected_item     = CCOpt.map (fun x -> CCList.get_at_idx x self#items) self#selected_index

      method disabled        = Js.to_bool self#select_element##.disabled
      method disable         = self#select_element##.disabled := Js._true
      method enable          = self#select_element##.disabled := Js._false
      method toggle_disabled = self#select_element##.disabled := Js.bool @@ not self#disabled

    end

end
