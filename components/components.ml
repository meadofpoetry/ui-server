open Widget
module Widgets = Markup

let of_dom el = Tyxml_js.Of_dom.of_element (el :> Dom_html.element Js.t)

module Avatar          = Avatar
module Widget          = Widget
module Button          = Button
module Checkbox        = Checkbox
module Dialog          = Dialog
module Fab             = Fab
module Form_field      = Form_field
module Icon            = Icon
module Icon_toggle     = Icon_toggle
module Layout_grid     = Layout_grid
module Linear_progress = Linear_progress
module List_           = List_
module Menu            = Menu
module Radio           = Radio
module Slider          = Slider
module Snackbar        = Snackbar
module Switch          = Switch
module Tabs            = Tabs
module Textfield       = Textfield
module Textarea        = Textarea

module Card = struct

  include Widgets.Card

  class type t = Dom_html.divElement

  let attach elt : t Js.t = Tyxml_js.To_dom.of_div elt

end

module Drawer = struct

  module Permanent = struct

    include Widgets.Drawer.Permanent

    class type t = Dom_html.element

  end

  module Persistent = struct

    include Widgets.Drawer.Persistent

    class type t =
      object
        method root__ : Dom_html.element Js.t Js.readonly_prop
        method open_  : bool Js.t Js.prop
      end

    type events =
      { opened : Dom_html.event Js.t Dom_events.Typ.typ
      ; closed : Dom_html.event Js.t Dom_events.Typ.typ
      }

    let events =
      { opened = Dom_events.Typ.make "MDCPersistentDrawer:open"
      ; closed = Dom_events.Typ.make "MDCPersistentDrawer:close"
      }

    let attach (elt : Html_types.aside Tyxml_js.Html.elt) : t Js.t =
      Js.Unsafe.global##.mdc##.drawer##.MDCPersistentDrawer##attachTo elt

  end

  module Temporary = struct

    include Widgets.Drawer.Temporary

    class type t =
      object
        method root__ : Dom_html.element Js.t Js.readonly_prop
        method open_  : bool Js.t Js.prop
      end

    type events =
      { opened : Dom_html.event Js.t Dom_events.Typ.typ
      ; closed : Dom_html.event Js.t Dom_events.Typ.typ
      }

    let events =
      { opened = Dom_events.Typ.make "MDCTemporaryDrawer:open"
      ; closed = Dom_events.Typ.make "MDCTemporaryDrawer:close"
      }

    let attach (elt : Html_types.aside Tyxml_js.Html.elt) : t Js.t =
      Js.Unsafe.global##.mdc##.drawer##.MDCTemporaryDrawer##attachTo elt

  end

end

module Elevation = struct

  include Widgets.Elevation

end

module Grid_list = struct

  include Widgets.Grid_list

  class type t = Dom_html.divElement

  let attach elt : t Js.t = Tyxml_js.To_dom.of_div elt

end

module Ripple = struct

  include Widgets.Ripple

  class type t =
    object
      method root__      : Dom_html.element Js.t Js.readonly_prop
      method activate_   : unit -> unit Js.meth
      method deactivate_ : unit -> unit Js.meth
      method layout_     : unit -> unit Js.meth
      method unbounded_  : bool Js.t Js.prop
    end

  let attach elt : t Js.t =
    Js.Unsafe.global##.mdc##.ripple##.MDCRipple##attachTo elt

end

module Rtl = struct

  include Widgets.Rtl

end

module Select = struct

  module Base = struct

    class type mdc =
      object
        method root__          : Dom_html.divElement Js.t Js.readonly_prop
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

    class item ?id ?selected ?disabled ?start_detail ?end_detail ~text () =

      let elt = Widgets.Select.Base.Item.create ?id ?selected ?disabled ~text ()
                |> Tyxml_js.To_dom.of_element in

      object(self)

        inherit [Dom_html.element Js.t] widget elt () as super

        method disabled        = (match self#get_attribute "aria-disabled" with
                                  | Some "true" -> true
                                  | _           -> false)
        method disable         = super#set_attribute "aria-disabled" "true"; super#set_attribute "tabindex" "-1"
        method enable          = super#remove_attribute "aria-disabled"; super#set_attribute "tabindex" "0"
        method toggle_disabled = if self#disabled then self#enable else self#disable

      end

    class t ?placeholder ~(items:item list) () =

      let elt = Widgets.Select.Base.create ?selected_text:placeholder
                                           ~items:(List.map (fun x -> Tyxml_js.Of_dom.of_element x#root) items)
                                           ()
                |> Tyxml_js.To_dom.of_element in

      object(self)

        inherit [Dom_html.element Js.t] widget elt ()

        val mdc : mdc Js.t = elt |> (fun x -> Js.Unsafe.global##.mdc##.select##.MDCSelect##attachTo x)
        val items = items

        method value          = Js.to_string mdc##.value (* id of item (if available) or text *)

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

  (* module Pure = struct
   * 
   *   module Item = struct
   * 
   *     class t ?disabled ?value ?group ~text () = object(self)
   * 
   *       inherit widget () as super
   * 
   *       val elt : Dom_html.optionElement Js.t = Widgets.Select.Pure.Item.create ?disabled ?value ~text ()
   *                                               |> Tyxml_js.To_dom.of_option
   * 
   *       method root = elt
   * 
   *       method disabled        = Js.to_bool self#root##.disabled
   *       method disable         = self#root##.disabled := Js._true
   *       method enable          = self#root##.disabled := Js._false
   *       method toggle_disabled = self#root##.disabled := Js.bool @@ not self#disabled
   * 
   *       method element = (elt :> Dom_html.element Js.t)
   * 
   *     end
   * 
   *   end
   * 
   *   module Group = struct
   * 
   *     class t ~label ~items () = object(self)
   * 
   *       inherit widget () as super
   * 
   *       val elt : Dom_html.optGroupElement Js.t =
   *         Widgets.Select.Pure.Item.create_group ~label
   *                                               ~items:(List.map (fun x -> Tyxml_js.Of_dom.of_option x) items)
   *                                               ()
   *         |> Tyxml_js.To_dom.of_optgroup
   * 
   *       method root = elt
   * 
   *       method disabled        = Js.to_bool self#root##.disabled
   *       method disable         = self#root##.disabled := Js._true
   *       method enable          = self#root##.disabled := Js._false
   *       method toggle_disabled = self#root##.disabled := Js.bool @@ not self#disabled
   * 
   *       method element = (elt :> Dom_html.element Js.t)
   * 
   *     end
   * 
   *   end
   * 
   *   class t ~(items : Group.t list) () =
   *   object(self)
   * 
   *     inherit widget ()
   * 
   *     val elt : Dom_html.selectElement Js.t =
   *       Widgets.Select.Pure.create ~items:(List.map (fun x -> Tyxml_js.Of_dom.of_optGroup x#root) items) ()
   *       |> Tyxml_js.To_dom.of_select
   * 
   *     method root = elt
   * 
   *     method value = Js.to_string self#root##.value
   * 
   *     method items = items
   *     method length = self#root##.length
   * 
   *     method selected_index    = self#root##.selectedIndex |> (fun x -> if x = -1 then None else Some x)
   *     method select_at_index i = self#root##.selectedIndex := i
   * 
   *     method disabled        = Js.to_bool self#root##.disabled
   *     method disable         = self#root##.disabled := Js._true
   *     method enable          = self#root##.disabled := Js._false
   *     method toggle_disabled = self#root##.disabled := Js.bool @@ not self#disabled
   * 
   *     method private element = (elt :> Dom_html.element Js.t)
   * 
   *   end
   * 
   * end *)

  module Multi = struct

    include Widgets.Select.Multi

    class type t = Dom_html.selectElement

    let attach elt : t Js.t = Tyxml_js.To_dom.of_select elt

  end

end

module Toolbar = struct

  include Widgets.Toolbar

  class type t =
    object
      method root__              : Dom_html.element Js.t Js.readonly_prop
      method fixedAdjustElement_ : Dom_html.element Js.t Js.prop
    end

  class type change_event =
    object
      inherit Dom_html.event
      method detail_ : < flexibleExpansionRatio_ : Js.number Js.t Js.readonly_prop > Js.t Js.readonly_prop
    end

  type events =
    { change : change_event Js.t Dom_events.Typ.typ
    }

  let events =
    { change = Dom_events.Typ.make "MDCToolbar:change"
    }

  let attach elt : t Js.t =
    Js.Unsafe.global##.mdc##.toolbar##.MDCToolbar##attachTo elt

end

module Typography = struct

  include Widgets.Typography

end
