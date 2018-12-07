open Js_of_ocaml
open Tyxml_js

module Markup = Components_markup.Menu.Make(Xml)(Svg)(Html)

module Item = struct

  class ['a] t ?secondary_text ?graphic ?meta ~value ~text () = object
    inherit ['a] Item_list.Item.t ?secondary_text
              ?graphic
              ?meta
              ~value
              ~text
              () as super

    method! init () =
      super#init ();
      super#set_attribute "role" "menuitem";
      super#set_attribute "tabindex" "0"

    method disabled = match super#get_attribute "aria-disabled" with
      | Some "true" -> true
      | _ -> false

    method set_disabled x =
      if x then (super#set_attribute "aria-disabled" "true";
                 super#set_attribute "tabindex" "-1")
      else super#remove_attribute "aria-disabled"
  end

end

let focus_index_to_js_obj (x : int) : < focusIndex : int Js.prop > Js.t =
  Js.Unsafe.(obj [| "focusIndex", inject x |])

(* TODO remove *)
class type mdc =
  object
    method open_ : bool Js.t Js.prop
    method hide : unit -> unit Js.meth
    method show : unit -> unit Js.meth
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
  ; cancel : Dom_html.event Js.t Dom_events.Typ.typ
  }

let events =
  { selected = Dom_events.Typ.make "MDCMenu:selected"
  ; cancel = Dom_events.Typ.make "MDCMenu:cancel"
  }

class ['a] t ?open_from ~(items:[ `Item of 'a Item.t | `Divider of Divider.t ] list) () =

  let list =
    new Item_list.t
      ~items:(List.map (function
                  | `Item x -> `Item (x : 'a Item.t :> 'a Item_list.Item.t)
                  | `Divider x -> `Divider x)
                items) () in
  let (elt : Dom_html.element Js.t) =
    Markup.create ?open_from ~list:(Widget.to_markup list) ()
    |> To_dom.of_div in
  let e_selected, e_selected_push = React.E.create () in
  let e_cancel, e_cancel_push = React.E.create () in
  object

    inherit Widget.t elt () as super

    val mdc : mdc Js.t = Js.Unsafe.global##.mdc##.menu##.MDCMenu##attachTo elt

    method! init () : unit =
      super#init ();
      list#set_attribute "role" "menu";
      list#set_attribute "aria-hidden" "true";
      list#add_class Markup.items_class;
      (* FIXME keep *)
      Dom_events.listen super#root events.selected (fun _ (e : event Js.t) ->
          let idx  = e##.detail##.index in
          let item = e##.detail##.item in
          e_selected_push (idx,item); false)  |> ignore;
      Dom_events.listen super#root events.cancel (fun _ _ ->
          e_cancel_push (); false) |> ignore

    method items = items
    method list = list
    method set_dense x = list#set_dense x

    method show () = mdc##show ()
    method show_with_focus x = mdc##show_focused (focus_index_to_js_obj x)
    method hide () = mdc##hide ()
    method opened = Js.to_bool mdc##.open_

    method e_selected = e_selected
    method e_cancel = e_cancel

  end

module Wrapper = struct

  type 'a menu = 'a t

  class ['a,'b] t ~(anchor : 'a) ~(menu : 'b menu) () = object

    inherit Widget.t (Html.div
                        ~a:[Html.a_class [Markup.anchor_class]]
                        [ Widget.to_markup anchor
                        ; Widget.to_markup menu ]
                      |> To_dom.of_div) ()
    method anchor = anchor
    method menu = menu
  end

end

let inject ~anchor ~(menu : 'a t) : unit =
  anchor#append_child menu;
  anchor#add_class Markup.anchor_class
