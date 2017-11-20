module Widgets = Common.Components.Make(Tyxml_js.Xml)(Tyxml_js.Svg)(Tyxml_js.Html)

                                       [@@@ocaml.warning "-60"]

let of_dom el = Tyxml_js.Of_dom.of_element (el :> Dom_html.element Js.t)

type rect =
  { top    : float
  ; right  : float
  ; bottom : float
  ; left   : float
  ; width  : float option
  ; height : float option
  }

class virtual widget () =
        object(self)

          method virtual private element : Dom_html.element Js.t

          method get_attribute a    = self#element##getAttribute (Js.string a)
                                      |> Js.Opt.to_option
                                      |> CCOpt.map Js.to_string
          method set_attribute a v  = self#element##setAttribute (Js.string a) (Js.string v)
          method remove_attribute a = self#element##removeAttribute (Js.string a)
          method has_attribute a    = self#element##hasAttribute (Js.string a)

          method id = Js.to_string self#element##.id
          method set_id id = self#element##.id := Js.string id

          method style = self#element##.style

          method class_string = Js.to_string @@ self#element##.className
          method set_class_string _classes = self#element##.className := (Js.string _classes)

          method add_class    _class = self#element##.classList##add (Js.string _class)
          method remove_class _class = self#element##.classList##remove (Js.string _class)
          method toggle_class _class = self#element##.classList##toggle (Js.string _class)
          method has_class    _class = Js.to_bool (self#element##.classList##contains (Js.string _class))

          method client_left   = self#element##.clientLeft
          method client_top    = self#element##.clientTop
          method client_width  = self#element##.clientWidth
          method client_height = self#element##.clientHeight

          method offset_left   = self#element##.offsetLeft
          method offset_top    = self#element##.offsetTop
          method offset_width  = self#element##.offsetWidth
          method offset_height = self#element##.offsetHeight

          method scroll_left   = self#element##.scrollLeft
          method scroll_top    = self#element##.scrollTop
          method scroll_width  = self#element##.scrollWidth
          method scroll_height = self#element##.scrollHeight

          method client_rect   = (self#element##getBoundingClientRect)
                                 |> (fun x -> { top    = x##.top
                                              ; right  = x##.right
                                              ; bottom = x##.bottom
                                              ; left   = x##.left
                                              ; width  = Js.Optdef.to_option x##.width
                                              ; height = Js.Optdef.to_option x##.height })

        end

class virtual input_widget () =
        object

          inherit widget ()

          method virtual input : Dom_html.inputElement Js.t

        end

module Button = struct

  class t ?raised ?icon ?ripple ~label () = object

    inherit widget () as super

    val elt : Dom_html.buttonElement Js.t =
      Widgets.Button.create ?raised ?icon ?ripple ~label ()
      |> Tyxml_js.To_dom.of_button

    method root = elt

    method unelevated = super#add_class Widgets.Button.unelevated_class
    method stroked    = super#add_class Widgets.Button.stroked_class
    method raised     = super#add_class Widgets.Button.raised_class
    method dense      = super#add_class Widgets.Button.dense_class
    method compact    = super#add_class Widgets.Button.compact_class

    method not_unelevated = super#remove_class Widgets.Button.unelevated_class
    method not_stroked    = super#remove_class Widgets.Button.stroked_class
    method not_raised     = super#remove_class Widgets.Button.raised_class
    method not_dense      = super#remove_class Widgets.Button.dense_class
    method not_compact    = super#remove_class Widgets.Button.compact_class

    method disabled = Js.to_bool elt##.disabled
    method disable  = elt##.disabled := Js._true
    method enable   = elt##.disabled := Js._false

    method private element = (elt :> Dom_html.element Js.t)

  end

end

module Card = struct

  include Widgets.Card

  class type t = Dom_html.divElement

  let attach elt : t Js.t = Tyxml_js.To_dom.of_div elt

end

module Checkbox = struct

  class type mdc =
    object
      method root__         : Dom_html.divElement Js.t Js.readonly_prop
      method checked_       : bool Js.t Js.prop
      method indeterminate_ : bool Js.t Js.prop
      method disabled_      : bool Js.t Js.prop
      method value_         : Js.js_string Js.t Js.prop
    end

  class t ?input_id () = object(self)

    inherit input_widget ()

    val mdc : mdc Js.t = Widgets.Checkbox.create ?input_id ()
                         |> (fun x -> Js.Unsafe.global##.mdc##.checkbox##.MDCCheckbox##attachTo x)

    method root    = mdc##.root__
    method input : Dom_html.inputElement Js.t =
      self#root##querySelector (Js.string ("." ^ Widgets.Checkbox.native_control_class))
      |> Js.Opt.to_option |> CCOpt.get_exn |> Js.Unsafe.coerce

    method checked        = Js.to_bool mdc##.checked_
    method check          = mdc##.checked_ := Js._true
    method uncheck        = mdc##.checked_ := Js._false
    method toggle_checked = mdc##.checked_ := Js.bool @@ not self#checked

    method disabled        = Js.to_bool mdc##.disabled_
    method disable         = mdc##.disabled_ := Js._true
    method enable          = mdc##.disabled_ := Js._false
    method toggle_disabled = mdc##.disabled_ := Js.bool @@ not self#disabled

    method indeterminate        = Js.to_bool mdc##.indeterminate_
    method set_indeterminate    = mdc##.indeterminate_ := Js._true
    method set_determinate      = mdc##.indeterminate_ := Js._false
    method toggle_indeterminate = mdc##.indeterminate_ := Js.bool @@ not self#indeterminate

    method value       = Js.to_string mdc##.value_
    method set_value v = mdc##.value_ := Js.string v

    method private element = (mdc##.root__ :> Dom_html.element Js.t)

  (* method listen event handler      = Dom_events.listen self#input event handler
   * method listen_root event handler = Dom_events.listen self#root event handler *)

  end

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

module Fab = struct

  class t ?ripple ~icon () = object

    inherit widget () as super

    val elt : Dom_html.buttonElement Js.t = Widgets.Fab.create ?ripple ~icon () |> Tyxml_js.To_dom.of_button

    method root = elt

    method mini     = super#add_class Widgets.Fab.mini_class
    method not_mini = super#remove_class Widgets.Fab.mini_class

    method private element = (elt :> Dom_html.element Js.t)

  end

end

module Form_field = struct

  class form_label ~for_id ~label = object

    val elt : Dom_html.labelElement Js.t = Widgets.Form_field.Label.create ~for_id ~label ()
                                           |> Tyxml_js.To_dom.of_label

    inherit widget ()

    method root = elt
    method private element = (elt :> Dom_html.element Js.t)

  end

  class ['a ] t ?align_end ~(input : 'a) ~label () =
    let label = new form_label
                    ~for_id:((input :> input_widget)#input##.id
                             |> Js.to_string)
                    ~label in
    object

      inherit widget ()

      val elt : Dom_html.divElement Js.t =
        Widgets.Form_field.create ?align_end
                                  ~input:(Tyxml_js.Of_dom.of_element input#root)
                                  ~label:(Tyxml_js.Of_dom.of_label label#root) ()
        |> Tyxml_js.To_dom.of_div

      method root  = elt
      method input = input
      method label = label

      method private element = (elt :> Dom_html.element Js.t)

    end

end

module Grid_list = struct

  include Widgets.Grid_list

  class type t = Dom_html.divElement

  let attach elt : t Js.t = Tyxml_js.To_dom.of_div elt

end

module Icon_toggle = struct

  type data = Widgets.Icon_toggle.data

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

  class type mdc =
    object
      method root__    : Dom_html.element Js.t Js.readonly_prop
      method on_       : bool Js.t Js.prop
      method disabled_ : bool Js.t Js.prop
    end

  class t ~on_data ~off_data () = object(self)

    inherit widget ()

    val mdc : mdc Js.t =
      Widgets.Icon_toggle.create ~on_data ~off_data ()
      |> Tyxml_js.To_dom.of_i
      |> (fun x -> Js.Unsafe.global##.mdc##.iconToggle##.MDCIconToggle##attachTo x)

    method root = mdc##.root__

    method disabled = Js.to_bool @@ mdc##.disabled_
    method disable  = mdc##.disabled_ := Js._true
    method enable   = mdc##.disabled_ := Js._false

    method is_on = Js.to_bool @@ mdc##.on_
    method on    = mdc##.on_ := Js._true
    method off   = mdc##.on_ := Js._false

    method private element = (self#root :> Dom_html.element Js.t)

  end

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

  class type mdc =
    object
      method root__    : Dom_html.divElement Js.t Js.readonly_prop
      method checked_  : bool Js.t Js.prop
      method disabled_ : bool Js.t Js.prop
      method value_    : Js.js_string Js.t Js.prop
    end

  class t ?input_id ?name () = object(self)

    inherit input_widget ()

    val mdc : mdc Js.t =
      Widgets.Radio.create ?input_id ?name ()
      |> Tyxml_js.To_dom.of_i
      |> (fun x -> Js.Unsafe.global##.mdc##.radio##.MDCRadio##attachTo x)

    method root  = mdc##.root__
    method input : Dom_html.inputElement Js.t =
      self#root##querySelector (Js.string ("." ^ Widgets.Radio.native_control_class))
      |> Js.Opt.to_option |> CCOpt.get_exn |> Js.Unsafe.coerce

    method disabled        = Js.to_bool mdc##.disabled_
    method disable         = mdc##.disabled_ := Js._true
    method enable          = mdc##.disabled_ := Js._false
    method toggle_disabled = mdc##.disabled_ := Js.bool @@ not self#disabled

    method checked        = Js.to_bool mdc##.checked_
    method check          = mdc##.checked_ := Js._true
    method uncheck        = mdc##.checked_ := Js._false
    method toggle_checked = mdc##.checked_ := Js.bool @@ not self#checked

    method value       = Js.to_string mdc##.value_
    method set_value v = mdc##.value_ := Js.string v

    method private element = (self#root :> Dom_html.element Js.t)

  end

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

    class item ?id ?selected ?disabled ?start_detail ?end_detail ~text () = object(self)

      inherit widget () as super

      val elt : Dom_html.element Js.t =
        Widgets.Select.Base.Item.create ?id ?selected ?disabled ~text ()
        |> Tyxml_js.To_dom.of_element

      method root = elt

      method disabled        = (match self#get_attribute "aria-disabled" with
                                | Some "true" -> true
                                | _           -> false)
      method disable         = super#set_attribute "aria-disabled" "true"; super#set_attribute "tabindex" "-1"
      method enable          = super#remove_attribute "aria-disabled"; super#set_attribute "tabindex" "0"
      method toggle_disabled = if self#disabled then self#enable else self#disable

      method element = (elt :> Dom_html.element Js.t)

    end

    class t ?placeholder ~(items:item list) () = object(self)

      inherit widget ()

      val mdc : mdc Js.t =
        Widgets.Select.Base.create ?selected_text:placeholder
                                   ~items:(List.map (fun x -> Tyxml_js.Of_dom.of_element x#root) items)
                                   ()
        |> Tyxml_js.To_dom.of_element
        |> (fun x -> Js.Unsafe.global##.mdc##.select##.MDCSelect##attachTo x)
      val items = items

      method root           = mdc##.root__

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

      method private element = mdc##.root__

    end

  end

  module Pure = struct

    module Item = struct

      class t ?disabled ?value ~text () = object(self)

        inherit widget () as super

        val elt : Dom_html.optionElement Js.t = Widgets.Select.Pure.Item.create ?disabled ?value ~text ()
                                                |> Tyxml_js.To_dom.of_option

        method root = elt

        method disabled        = Js.to_bool self#root##.disabled
        method disable         = self#root##.disabled := Js._true
        method enable          = self#root##.disabled := Js._false
        method toggle_disabled = self#root##.disabled := Js.bool @@ not self#disabled

        method element = (elt :> Dom_html.element Js.t)

      end

    end

    module Group = struct

      class t ~label ~items () = object(self)

        inherit widget () as super

        val elt : Dom_html.optGroupElement Js.t =
          Widgets.Select.Pure.Item.create_group ~label
                                                ~items:(List.map (fun x -> Tyxml_js.Of_dom.of_option x) items)
                                                ()
          |> Tyxml_js.To_dom.of_optgroup

        method root = elt

        method disabled        = Js.to_bool self#root##.disabled
        method disable         = self#root##.disabled := Js._true
        method enable          = self#root##.disabled := Js._false
        method toggle_disabled = self#root##.disabled := Js.bool @@ not self#disabled

        method element = (elt :> Dom_html.element Js.t)

      end

    end

    class t ~(items : Group.t list) () = object(self)

      inherit widget ()

      val elt : Dom_html.selectElement Js.t =
        Widgets.Select.Pure.create ~items:(List.map (fun x -> Tyxml_js.Of_dom.of_optGroup x#root) items) ()
        |> Tyxml_js.To_dom.of_select

      method root = elt

      method value = Js.to_string self#root##.value

      method items = items
      method length = self#root##.length

      method selected_index    = self#root##.selectedIndex |> (fun x -> if x = -1 then None else Some x)
      method select_at_index i = self#root##.selectedIndex := i

      method disabled        = Js.to_bool self#root##.disabled
      method disable         = self#root##.disabled := Js._true
      method enable          = self#root##.disabled := Js._false
      method toggle_disabled = self#root##.disabled := Js.bool @@ not self#disabled

      method private element = (elt :> Dom_html.element Js.t)

    end

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

  class t ?input_id () = object(self)

    inherit input_widget ()

    val elt : Dom_html.divElement Js.t = Widgets.Switch.create ?input_id () |> Tyxml_js.To_dom.of_div

    method root = elt
    method input = elt##querySelector (Js.string ("." ^ Widgets.Switch.native_control_class))
                   |> Js.Opt.to_option |> CCOpt.get_exn |> Js.Unsafe.coerce

    method disabled        = Js.to_bool self#input##.disabled
    method disable         = self#input##.disabled := Js._true
    method enable          = self#input##.disabled := Js._false
    method toggle_disabled = self#input##.disabled := Js.bool @@ not self#disabled

    method checked        = Js.to_bool self#input##.checked
    method check          = self#input##.checked := Js._true
    method uncheck        = self#input##.checked := Js._false
    method toggle_checked = self#input##.disabled := Js.bool @@ not self#checked

    method value       = Js.to_string self#input##.value
    method set_value v = self#input##.value := Js.string v

    method private element = (elt :> Dom_html.element Js.t)

  end

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
    Js.Unsafe.global##.mdc##.textField##.MDCTextField##attachTo elt

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
