module Widgets = Common.Components.Make(Tyxml_js.Xml)(Tyxml_js.Svg)(Tyxml_js.Html)

                                       [@@@ocaml.warning "-60"]

let of_dom el = Tyxml_js.Of_dom.of_element (el :> Dom_html.element Js.t)

module Button = struct

  include Widgets.Button

  class type t = Dom_html.buttonElement

  let attach elt : t Js.t = Tyxml_js.To_dom.of_button elt

end

module Card = struct

  include Widgets.Card

  class type t = Dom_html.divElement

  let attach elt : t Js.t = Tyxml_js.To_dom.of_div elt

end

module Checkbox = struct

  include Widgets.Checkbox

  class type t =
    object
      method root__         : Dom_html.divElement Js.t Js.readonly_prop
      method checked_       : bool Js.t Js.prop
      method indeterminate_ : bool Js.t Js.prop
      method disabled_      : bool Js.t Js.prop
      method value_         : Js.js_string Js.prop
    end

  let attach elt : t Js.t =
    Js.Unsafe.global##.mdc##.checkbox##.MDCCheckbox##attachTo elt

end

module Dialog = struct

  include Widgets.Dialog

  class type foundation =
    object
      method open_    : unit -> unit Js.meth
      method close_   : unit -> unit Js.meth
      method is_open_ : unit -> bool Js.t Js.meth
      method accept_  : bool Js.t -> unit Js.meth
      method cancel_  : bool Js.t -> unit Js.meth
    end

  class type t =
    object
      method root__       : Dom_html.element Js.t Js.readonly_prop
      method foundation__ : foundation Js.t Js.readonly_prop
      method open_        : bool Js.t Js.prop
      method show_        : unit -> unit Js.meth
      method close_       : unit -> unit Js.meth
    end

  type events =
    { accept : Dom_html.event Js.t Dom_events.Typ.typ
    ; cancel : Dom_html.event Js.t Dom_events.Typ.typ
    }

  let events = { accept = Dom_events.Typ.make "MDCDialog:accept"
               ; cancel = Dom_events.Typ.make "MDCDialog:cancel"
               }

  let attach (elt : Html_types.aside Tyxml_js.Html.elt) : t Js.t =
    Js.Unsafe.global##.mdc##.dialog##.MDCDialog##attachTo elt

end

module Drawer = struct

  include Widgets.Drawer

end

module Elevation = struct

  include Widgets.Elevation

end

module Fab = struct

  include Widgets.Fab

  class type t = Dom_html.buttonElement

  let attach elt : t Js.t = Tyxml_js.To_dom.of_button elt

end

module Form_field = struct

  include Widgets.Form_field

  class type t = Dom_html.divElement

  let attach elt : t Js.t = Tyxml_js.To_dom.of_div elt

end

module Grid_list = struct

  include Widgets.Grid_list

  class type t = Dom_html.divElement

  let attach elt : t Js.t = Tyxml_js.To_dom.of_div elt

end

module Icon_toggle = struct

  include Widgets.Icon_toggle

  class type component =
    object
      method on_       : bool Js.t Js.prop
      method disabled_ : bool Js.t Js.prop
    end

  class type t =
    object
      method root__     : Dom_html.element Js.t Js.readonly_prop
      method component_ : component Js.t Js.readonly_prop
    end

  class type change_event =
    object
      inherit Dom_html.event
      method detail_ : < isOn : bool Js.t Js.readonly_prop > Js.t Js.readonly_prop
    end

  type events =
    { change : change_event Js.t Dom_events.Typ.typ
    }

  let events =
    { change = Dom_events.Typ.make "MDCIconToggle:change"
    }

  let attach elt : t Js.t =
    Js.Unsafe.global##.mdc##.iconToggle##.MDCIconToggle##attachTo elt

end

module Layout_grid = struct

  include Widgets.Layout_grid

  class type t = Dom_html.divElement

  let attach elt : t Js.t = Tyxml_js.To_dom.of_div elt

end

module Linear_progress = struct

  include Widgets.Linear_progress

  class type t =
    object
      method root__       : Dom_html.divElement Js.t Js.readonly_prop
      method determinate_ : bool Js.t Js.writeonly_prop
      method progress_    : Js.number Js.t Js.writeonly_prop
      method buffer_      : Js.number Js.t Js.writeonly_prop
      method reverse_     : bool Js.t Js.writeonly_prop
      method open_        : unit -> unit Js.meth
      method close_       : unit -> unit Js.meth
    end

  let attach elt : t Js.t =
    Js.Unsafe.global##.mdc##.linearProgress##.MDCLinearProgress##attachTo elt

end

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

module Radio = struct

  include Widgets.Radio

  class type t =
    object
      method root__    : Dom_html.divElement Js.t Js.readonly_prop
      method checked_  : bool Js.t Js.prop
      method disabled_ : bool Js.t Js.prop
      method value_    : Js.js_string Js.prop
    end

  let attach elt : t Js.t =
    Js.Unsafe.global##.mdc##.radio##.MDCRadio##attachTo elt

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

  let base_class = Widgets.Select.base_class

  module Base = struct

    include Widgets.Select.Base

    class type t =
      object
        method root__           : Dom_html.divElement Js.t Js.readonly_prop
        method value_           : Js.js_string Js.t Js.readonly_prop
        method options_         : Dom_html.element Js.js_array Js.t Js.readonly_prop
        method selectedIndex_   : Js.number Js.t Js.prop
        method selectedOptions_ : Dom_html.element Js.js_array Js.t Js.readonly_prop
        method disabled_        : bool Js.t Js.prop
        method item_            : (Js.number Js.t -> Dom_html.element Js.t Js.opt) Js.meth
        method nameditem_       : (Js.js_string Js.t -> Dom_html.element Js.t Js.opt) Js.meth
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
      { change = Dom_events.Typ.make "MDCSelect:change"
      }

    let attach elt : t Js.t =
      Js.Unsafe.global##.mdc##.select##.MDCSelect##attachTo elt

  end

  module Pure = struct

    include Widgets.Select.Pure

    class type t = Dom_html.selectElement

    let attach elt : t Js.t = Tyxml_js.To_dom.of_select elt

  end

  module Multi = struct

    include Widgets.Select.Multi

    class type t = Dom_html.selectElement

    let attach elt : t Js.t = Tyxml_js.To_dom.of_select elt

  end

end

module Slider = struct

  include Widgets.Slider

  class type t =
    object
      method root__         : Dom_html.divElement Js.t Js.readonly_prop
      method value_         : Js.number Js.t Js.prop
      method min_           : Js.number Js.t Js.prop
      method max_           : Js.number Js.t Js.prop
      method step_          : Js.number Js.t Js.prop
      method disabled_      : bool Js.t Js.prop
      method layout_        : unit -> unit Js.meth
      method stepUp_        : unit -> unit Js.meth
      method stepDown_      : unit -> unit Js.meth
      method stepUp_value   : Js.number Js.t -> unit Js.meth
      method stepDown_value : Js.number Js.t -> unit Js.meth
    end

  class type event =
    object
      inherit Dom_html.event
      method detail_ : t Js.t Js.readonly_prop
    end

  type events =
    { input  : event Js.t Dom_events.Typ.typ
    ; change : event Js.t Dom_events.Typ.typ
    }

  let events =
    { input  = Dom_events.Typ.make "MDCSlider:input"
    ; change = Dom_events.Typ.make "MDCSlider:change"
    }

  let attach elt : t Js.t =
    Js.Unsafe.global##.mdc##.slider##.MDCSlider##attachTo elt

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

module Switch = struct

  include Widgets.Switch

  class type t =
    object
      inherit Dom_html.element
      method is_checked_   : unit -> bool Js.t Js.meth
      method set_checked_  : bool Js.t -> unit Js.meth
      method is_disabled_  : unit -> bool Js.t Js.meth
      method set_disabled_ : bool Js.t -> unit Js.meth
      method get_value_    : unit -> Js.js_string Js.meth
      method set_value_    : Js.js_string -> unit Js.meth
    end

  let get_input (switch : t Js.t) : Dom_html.inputElement Js.t Js.opt =
    switch##querySelector (Js.string ("." ^ native_control_class))
    |> Js.Opt.to_option
    |> (function
        | Some x -> Js.Opt.return @@ Js.Unsafe.coerce x
        | None   -> Js.Opt.empty)

  let attach elt : t Js.t =
    let (elt : t Js.t) = elt |> Tyxml_js.To_dom.of_element |> Js.Unsafe.coerce in
    let set    = fun (x : t Js.t) (name : string) f -> Js.Unsafe.set x name f in
    set elt "is_checked"   @@ Js.wrap_callback (fun () -> match Js.Opt.to_option @@ get_input elt with
                                                          | Some nc -> nc##.checked
                                                          | None    -> Js._false);
    set elt "set_checked"  @@ Js.wrap_callback (fun x -> match Js.Opt.to_option @@ get_input elt with
                                                         | Some nc -> nc##.checked := x
                                                         | None    -> ());
    set elt "is_disabled"  @@ Js.wrap_callback (fun () -> match Js.Opt.to_option @@ get_input elt with
                                                          | Some nc -> nc##.disabled
                                                          | None    -> Js._false);
    set elt "set_disabled" @@ Js.wrap_callback (fun x -> match Js.Opt.to_option @@ get_input elt with
                                                         | Some nc -> nc##.disabled := x
                                                         | None    -> ());
    set elt "get_value"    @@ Js.wrap_callback (fun () -> match Js.Opt.to_option @@ get_input elt with
                                                          | Some nc -> nc##.value
                                                          | None    -> Js.string "");
    set elt "set_value"    @@ Js.wrap_callback (fun x -> match Js.Opt.to_option @@ get_input elt with
                                                         | Some nc -> nc##.value := x
                                                         | None    -> ());
    elt

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

module Textfield = struct

  include Widgets.Textfield

  class type t =
    object
      method root__           : Dom_html.divElement Js.t Js.readonly_prop
      method helptextElement_ : Dom_html.element Js.t Js.prop
      method disabled_        : bool Js.t Js.prop
      method valid_           : bool Js.t Js.writeonly_prop
      method ripple           : Ripple.t Js.t Js.prop
    end

  type events =
    { icon : Dom_html.event Js.t Dom_events.Typ.typ
    }

  let events =
    { icon = Dom_events.Typ.make "MDCTextfield:icon"
    }

  let get_input (textfield : Dom_html.divElement Js.t) : Dom_html.inputElement Js.t Js.opt =
    textfield##querySelector (Js.string ("." ^ input_class))
    |> Js.Opt.to_option
    |> (function
        | Some x -> Js.Opt.return @@ Js.Unsafe.coerce x
        | None   -> Js.Opt.empty)

  let attach elt : t Js.t =
    Js.Unsafe.global##.mdc##.textfield##.MDCTextfield##attachTo elt

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
