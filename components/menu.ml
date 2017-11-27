open Widget
open Tyxml_js
module Menu = Markup.Menu

module Divider = List_.Divider

module Item = struct

  class t ?secondary_text ?start_detail ?end_detail ~text () = object(self)

    inherit List_.Item.t ?secondary_text ?start_detail ?end_detail ~text () as super

    method disabled        = (match super#get_attribute "aria-disabled" with
                              | Some "true" -> true
                              | _           -> false)
    method disable         = super#set_attribute "aria-disabled" "true";
                             super#set_attribute "tabindex" "-1"
    method enable          = super#remove_attribute "aria-disabled"
    method toggle_disabled = if self#disabled then self#enable else self#disable

    initializer
      super#set_attribute "role" "menuitem";
      super#set_attribute "tabindex" "0"

  end

end

let focus_index_to_js_obj (x : int) : < focusIndex : int Js.prop > Js.t =
  Js.Unsafe.(obj [| "focusIndex", inject x |])

class type mdc =
  object
    method open_        : bool Js.t Js.prop
    method hide         : unit -> unit Js.meth
    method show         : unit -> unit Js.meth
    method show_focused : < focusIndex : int Js.prop > Js.t -> unit Js.meth
  end

class type event =
  object
    inherit Dom_html.event
    method detail : < item  : Dom_html.element Js.t Js.readonly_prop;
           index : int Js.readonly_prop > Js.t Js.readonly_prop
  end

type events =
  { selected : event Js.t Dom_events.Typ.typ
  ; cancel   : Dom_html.event Js.t Dom_events.Typ.typ
  }

let events =
  { selected = Dom_events.Typ.make "MDCSimpleMenu:selected"
  ; cancel   = Dom_events.Typ.make "MDCSimpleMenu:cancel"
  }

class t ?open_from ~(items:[ `Item of Item.t | `Divider of Divider.t ] list) () =

  let list = new List_.t ~items:(List.map (function
                                           | `Item x    -> `Item (x : Item.t :> List_.Item.t)
                                           | `Divider x -> `Divider x)
                                          items) () in

  let () = list#set_attribute "role" "menu";
           list#set_attribute "aria-hidden" "true";
           list#add_class Menu.items_class in

  let elt = Menu.create ?open_from ~list:(widget_to_markup list) () |> To_dom.of_div in

  object

    inherit widget elt ()

    val mdc : mdc Js.t = Js.Unsafe.global##.mdc##.menu##.MDCSimpleMenu##attachTo elt

    method items = items

    method list      = list
    method dense     = list#dense
    method not_dense = list#not_dense

    method show              = mdc##show ()
    method show_with_focus x = mdc##show_focused (focus_index_to_js_obj x)
    method hide              = mdc##hide ()
    method is_opened         = Js.to_bool mdc##.open_

  end

module Wrapper = struct

  type menu = t

  class ['a] t ~(anchor:'a) ~(menu:menu) () = object

    inherit widget (Html.div ~a:[Html.a_class [Menu.anchor_class]]
                             [ widget_to_markup anchor; widget_to_markup menu ]
                    |> To_dom.of_div) ()
    method anchor = anchor
    method menu   = menu
  end

end

let inject ~anchor ~(menu:t) =
  Dom.appendChild anchor#root menu#root;
  anchor#add_class Menu.anchor_class
