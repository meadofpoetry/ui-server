open Widget
open Markup
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

    class t ?id ?selected ?disabled ?start_detail ?end_detail ~text () =

      let elt = Select.Base.Item.create ?id ?selected ?disabled ~text ()
                |> To_dom.of_element in

      object(self)

        inherit [Dom_html.element Js.t] widget elt () as super

        method disabled        = (match self#get_attribute "aria-disabled" with
                                  | Some "true" -> true
                                  | _           -> false)
        method disable         = super#set_attribute "aria-disabled" "true"; super#set_attribute "tabindex" "-1"
        method enable          = super#remove_attribute "aria-disabled"; super#set_attribute "tabindex" "0"
        method toggle_disabled = if self#disabled then self#enable else self#disable

      end

  end

  class t ?placeholder ~(items:Item.t list) () =

    let elt = Select.Base.create ?selected_text:placeholder
                                 ~items:(List.map (fun x -> Of_dom.of_element x#root) items)
                                 ()
              |> To_dom.of_element in

    object(self)

      inherit [Dom_html.element Js.t] widget elt ()

      val mdc : mdc Js.t = elt |> (fun x -> Js.Unsafe.global##.mdc##.select##.MDCSelect##attachTo x)
      val items = items

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

    class t ?disabled ?value ~text () = object(self)

      inherit [Dom_html.optionElement Js.t] widget (Select.Pure.Item.create ?disabled ?value ~text ()
                                                    |> Tyxml_js.To_dom.of_option) () as super

      method text        = Js.to_string super#root##.text
      method value       = Js.to_string super#root##.value
      method set_value v = super#root##.value := Js.string v
      method index       = super#root##.index

      method selected    = Js.to_bool super#root##.selected
      method select      = super#root##.selected := Js._true
      method deselect    = super#root##.selected := Js._false

      method disabled        = Js.to_bool super#root##.disabled
      method disable         = super#root##.disabled := Js._true
      method enable          = super#root##.disabled := Js._false
      method toggle_disabled = super#root##.disabled := Js.bool @@ not self#disabled

    end

  end

  module Group = struct

    class t ~label ~(items:Item.t list) () =

      let item_elts = List.map (fun x -> Of_dom.of_option x#root) items in

      object(self)

        inherit [Dom_html.optGroupElement Js.t] widget (Select.Pure.Item.create_group ~label ~items:item_elts ()
                                                        |> To_dom.of_optgroup) () as super

        method items           = items
        method label           = Js.to_string super#root##.label
        method set_label s     = super#root##.label := Js.string s

        method disabled        = Js.to_bool super#root##.disabled
        method disable         = super#root##.disabled := Js._true
        method enable          = super#root##.disabled := Js._false
        method toggle_disabled = super#root##.disabled := Js.bool @@ not self#disabled

      end

  end

  class t ~(items : [ `Item of Item.t | `Group of Group.t ] list) () =

    let item_elts = List.map (function
                              | `Group g -> Of_dom.of_element g#element
                              | `Item i  -> Of_dom.of_element i#element) items in

  object(self)

    inherit [Dom_html.selectElement Js.t] widget (Select.Pure.create ~items:item_elts ()
                                                  |> To_dom.of_select) () as super

    method value = Js.to_string super#root##.value

    method items = CCList.fold_left (fun acc x -> match x with
                                                  | `Group g -> acc @ g#items
                                                  | `Item i  -> acc @ [i])
                                    [] items

    method length = super#root##.length
    method item n = CCList.get_at_idx n self#items

    method selected_index    = super#root##.selectedIndex |> (fun x -> if x = -1 then None else Some x)
    method select_at_index i = super#root##.selectedIndex := i
    method selected_item     = CCOpt.map (fun x -> CCList.get_at_idx x self#items) self#selected_index

    method disabled        = Js.to_bool super#root##.disabled
    method disable         = super#root##.disabled := Js._true
    method enable          = super#root##.disabled := Js._false
    method toggle_disabled = super#root##.disabled := Js.bool @@ not self#disabled

  end

end

module Multi = struct

  include Select.Multi

  class type t = Dom_html.selectElement

  let attach elt : t Js.t = To_dom.of_select elt

end
