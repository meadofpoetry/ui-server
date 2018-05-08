module Divider = Item_list.Divider

module Item = struct

  class t ?secondary_text ?start_detail ?end_detail ~text () = object
    inherit Item_list.Item.t ?secondary_text ?start_detail ?end_detail ~text () as super

    method get_disabled   = (match super#get_attribute "aria-disabled" with
                             | Some "true" -> true
                             | _           -> false)
    method set_disabled x = if x then (super#set_attribute "aria-disabled" "true";
                                       super#set_attribute "tabindex" "-1")
                            else super#remove_attribute "aria-disabled"

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
  { selected = Dom_events.Typ.make "MDCMenu:selected"
  ; cancel   = Dom_events.Typ.make "MDCMenu:cancel"
  }

class t ?open_from ~(items:[ `Item of Item.t | `Divider of Divider.t ] list) () =

  let list = new Item_list.t ~items:(List.map (function
                                         | `Item x    -> `Item (x : Item.t :> Item_list.Item.t)
                                         | `Divider x -> `Divider x)
                                       items) () in

  let () = list#set_attribute "role" "menu";
           list#set_attribute "aria-hidden" "true";
           list#add_class Markup.Menu.items_class in

  let elt = Markup.Menu.create ?open_from ~list:(Widget.widget_to_markup list) () |> Tyxml_js.To_dom.of_div in
  let e_selected,e_selected_push = React.E.create () in
  let e_cancel,e_cancel_push     = React.E.create () in
  object

    inherit Widget.widget elt () as super

    val mdc : mdc Js.t = Js.Unsafe.global##.mdc##.menu##.MDCMenu##attachTo elt

    method items   = items
    method list    = list
    method set_dense x = list#set_dense x

    method show ()           = mdc##show ()
    method show_with_focus x = mdc##show_focused (focus_index_to_js_obj x)
    method hide ()           = mdc##hide ()
    method opened            = Js.to_bool mdc##.open_

    method e_selected = e_selected
    method e_cancel   = e_cancel

    initializer
      Dom_events.listen super#root events.selected
                        (fun _ (e:event Js.t) -> let idx  = e##.detail##.index in
                                                 let item = e##.detail##.item in
                                                 e_selected_push (idx,item); false)  |> ignore;
      Dom_events.listen super#root events.cancel
                        (fun _ _ -> e_cancel_push (); false) |> ignore
  end

module Wrapper = struct

  type menu = t

  class ['a] t ~(anchor:'a) ~(menu:menu) () = object

    inherit Widget.widget (Tyxml_js.Html.div ~a:[Tyxml_js.Html.a_class [Markup.Menu.anchor_class]]
                                             [ Widget.widget_to_markup anchor
                                             ; Widget.widget_to_markup menu ]
                           |> Tyxml_js.To_dom.of_div) ()
    method anchor = anchor
    method menu   = menu
  end

end

let inject ~anchor ~(menu:t) =
  Dom.appendChild anchor#root menu#root;
  anchor#add_class Markup.Menu.anchor_class
