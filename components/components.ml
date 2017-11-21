module Widgets = Common.Components.Make(Tyxml_js.Xml)(Tyxml_js.Svg)(Tyxml_js.Html)

                   [@@@ocaml.warning "-60"]

open Widget

let of_dom el = Tyxml_js.Of_dom.of_element (el :> Dom_html.element Js.t)

module Widget          = Widget
module Button          = Button
module Checkbox        = Checkbox
module Dialog          = Dialog
module Fab             = Fab
module Form_field      = Form_field
module Icon_toggle     = Icon_toggle
module Layout_grid     = Layout_grid
module Linear_progress = Linear_progress
module Radio           = Radio
module Slider          = Slider
module Switch          = Switch
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

(* module Layout_grid = struct
 * 
 *   include Widgets.Layout_grid
 * 
 *   class type t = Dom_html.divElement
 * 
 *   let attach elt : t Js.t = Tyxml_js.To_dom.of_div elt
 * 
 * end *)

module List_ = struct

  include Widgets.List_

  class type t = Dom_html.element

  let attach elt : t Js.t = Tyxml_js.To_dom.of_element elt

end

module Menu = struct

  include Widgets.Menu

  let focus_index_to_js_obj x : < focusIndex : Js.number Js.t Js.prop > Js.t =
    Js.Unsafe.(obj [| "focusIndex", inject @@ Js.number_of_float (float_of_int x) |])

  class type t =
    object
      method root__       : Dom_html.divElement Js.t Js.readonly_prop
      method open_        : bool Js.t Js.prop
      method hide_        : unit -> unit Js.meth
      method show_        : unit -> unit Js.meth
      method show_focused : < focusIndex : Js.number Js.t Js.prop > Js.t -> unit Js.meth
    end

  class type event =
    object
      inherit Dom_html.event
      method detail_ : < item_  : Dom_html.element Js.t Js.readonly_prop;
                         index_ : Js.number Js.t Js.readonly_prop > Js.t Js.readonly_prop
    end

  type events =
    { selected : event Js.t Dom_events.Typ.typ
    ; cancel   : Dom_html.event Js.t Dom_events.Typ.typ
    }

  let events =
    { selected = Dom_events.Typ.make "MDCSimpleMenu:selected"
    ; cancel   = Dom_events.Typ.make "MDCSimpleMenu:cancel"
    }

  let attach elt : t Js.t =
    Js.Unsafe.global##.mdc##.menu##.MDCSimpleMenu##attachTo elt


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

module Snackbar = struct

  include Widgets.Snackbar

  type data =
    { message          : string
    ; timeout          : int option
    ; action           : action option
    ; multiline        : multiline option
    }
   and action    = { handler : unit -> unit
                   ; text    : string
                   }
   and multiline = { enable           : bool
                   ; action_on_bottom : bool
                   }

  class type data_obj =
    object
      method actionHandler  : (unit -> unit) Js.optdef Js.readonly_prop
      method actionOnBottom : bool Js.t Js.optdef Js.readonly_prop
      method actionText     : Js.js_string Js.t Js.optdef Js.readonly_prop
      method message        : Js.js_string Js.t Js.readonly_prop
      method multiline      : bool Js.t Js.optdef Js.readonly_prop
      method timeout        : Js.number Js.t Js.optdef Js.readonly_prop
    end

  let data_to_js_obj x : data_obj Js.t =
    object%js
      val message        = Js.string x.message
      val timeout        = CCOpt.map (fun x -> Js.number_of_float @@ float_of_int x) x.timeout |> Js.Optdef.option
      val actionHandler  = CCOpt.map (fun x -> x.handler) x.action |> Js.Optdef.option
      val actionText     = CCOpt.map (fun x -> Js.string x.text) x.action |> Js.Optdef.option
      val multiline      = CCOpt.map (fun x -> Js.bool x.enable) x.multiline |> Js.Optdef.option
      val actionOnBottom = CCOpt.map (fun x -> Js.bool x.action_on_bottom) x.multiline |> Js.Optdef.option
    end

  class type t =
    object
      method root__             : Dom_html.divElement Js.t Js.readonly_prop
      method show_              : data_obj Js.t -> unit Js.meth
      method dismissesOnAction_ : bool Js.t Js.prop
    end

  let attach elt : t Js.t =
    Js.Unsafe.global##.mdc##.snackbar##.MDCSnackbar##attachTo elt

end

module Tabs = struct

  module Tab = struct

    include Widgets.Tabs.Tab

    class type t =
      object
        method root__                 : Dom_html.element Js.t Js.readonly_prop
        method computedWidth_         : Js.number Js.t Js.readonly_prop
        method computedLeft_          : Js.number Js.t Js.readonly_prop
        method isActive_              : bool Js.t Js.prop
        method preventDefaultOnClick_ : bool Js.t Js.prop
      end

    class type selected_event =
      object
        inherit Dom_html.event
        method detail_ : < tab_ : t Js.t Js.readonly_prop > Js.t Js.readonly_prop
      end

    type events =
      { selected : selected_event Js.t Dom_events.Typ.typ
      }

    let events =
      { selected = Dom_events.Typ.make "MDCTab:selected"
      }

    let attach elt : t Js.t =
      Js.Unsafe.global##.mdc##.tabs##.MDCTab##attachTo elt

  end

  module Tab_bar = struct

    include Widgets.Tabs.Tab_bar

    class type t =
      object
        method root__          : Dom_html.element Js.t Js.readonly_prop
        method tabs_           : Tab.t Js.t Js.js_array Js.readonly_prop
        method activeTab_      : Tab.t Js.t Js.prop
        method activeTabIndex_ : Js.number Js.t Js.prop
      end

    class type change_event =
      object
        inherit Dom_html.event
        method detail_ : t Js.t Js.readonly_prop
      end

    type events =
      { change : change_event Js.t Dom_events.Typ.typ
      }

    let events =
      { change = Dom_events.Typ.make "MDCTabBar:change"
      }

    let attach elt : t Js.t =
      Js.Unsafe.global##.mdc##.tabs##.MDCTabBar##attachTo elt

  end

  module Scroller = struct

    include Widgets.Tabs.Scroller

    class type t =
      object
        method root__  : Dom_html.element Js.t Js.readonly_prop
        method tabBar_ : Tab_bar.t Js.t Js.readonly_prop
      end

    let attach elt : t Js.t =
      Js.Unsafe.global##.mdc##.tabs##.MDCTabBarScroller##attachTo elt

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
