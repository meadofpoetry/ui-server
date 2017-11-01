module Widgets = Common.Components.Make(Tyxml_js.Xml)(Tyxml_js.Svg)(Tyxml_js.Html)

                                       [@@@ocaml.warning "-60"]

let of_dom el = Tyxml_js.Of_dom.of_element (Js.Unsafe.coerce el)

module Button = struct

  include Widgets.Button

  class type t =
    object
      inherit Dom_html.element
    end

  let create ?classes ?id ?style ?disabled ?color_scheme ?raised
             ?ripple ?dense ?compact ?label ?onclick ?attrs () =
    create ?classes ?id ?style ?disabled ?color_scheme ?raised
           ?ripple ?dense ?compact ?label ?onclick ?attrs ()
    |> Tyxml_js.To_dom.of_element

end

module Card = struct

  include Widgets.Card

  class type t =
    object
      inherit Dom_html.element
    end

  let create ?sections ?id ?style ?classes ?attrs () : t Js.t =
    Widgets.Card.create ?sections ?id ?style ?classes ?attrs ()
    |> Tyxml_js.To_dom.of_element

end

module Checkbox = struct

  include Widgets.Checkbox

  class type component =
    object
      method checked_       : bool Js.t Js.prop
      method indeterminate_ : bool Js.t Js.prop
      method disabled_      : bool Js.t Js.prop
      method value_         : Js.js_string Js.prop
    end

  class type t =
    object
      inherit Dom_html.element
      method component_         : component Js.t Js.readonly_prop
      method is_checked_        : unit -> bool Js.t Js.meth
      method set_checked_       : bool Js.t -> unit Js.meth
      method is_indeterminate_  : unit -> bool Js.t Js.meth
      method set_indeterminate_ : bool Js.t -> unit Js.meth
      method is_disabled_       : unit -> bool Js.t Js.meth
      method set_disabled_      : bool Js.t -> unit Js.meth
      method get_value_         : unit -> Js.js_string Js.meth
      method set_value_         : Js.js_string -> unit Js.meth
    end

  let create ?classes ?style ?id ?input_id ?disabled ?js ?checked ?attrs () : t Js.t =
    let open Js.Unsafe in
    let (elt : t Js.t) = create ?classes ?style ?id ?input_id ?disabled ?js ?checked ?attrs ()
                         |> Tyxml_js.To_dom.of_element
                         |> Js.Unsafe.coerce in
    let set = fun (x : t Js.t) (name : string) f -> Js.Unsafe.set x name f in
    set elt "component" @@ fun_call (js_expr "mdc.checkbox.MDCCheckbox.attachTo") [| inject elt |];
    set elt "is_checked"        @@ Js.wrap_callback (fun () -> elt##.component_##.checked_);
    set elt "set_checked"       @@ Js.wrap_callback (fun x  -> elt##.component_##.checked_ := x);
    set elt "is_indeterminate"  @@ Js.wrap_callback (fun () -> elt##.component_##.indeterminate_);
    set elt "set_indeterminate" @@ Js.wrap_callback (fun x  -> elt##.component_##.indeterminate_ := x);
    set elt "is_disabled"       @@ Js.wrap_callback (fun () -> elt##.component_##.disabled_);
    set elt "set_disabled"      @@ Js.wrap_callback (fun x  -> elt##.component_##.disabled_ := x);
    set elt "get_value"         @@ Js.wrap_callback (fun () -> elt##.component_##.value_);
    set elt "set_value"         @@ Js.wrap_callback (fun x  -> elt##.component_##.value_ := x);
    elt

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

  class type component =
    object
      method foundation_ : foundation Js.t Js.readonly_prop
      method open_       : bool Js.t Js.prop
      method show_       : unit -> unit Js.meth
      method close_      : unit -> unit Js.meth
    end

  class type t =
    object
      inherit Dom_html.element
      method component_ : component Js.t Js.readonly_prop
      method show_      : unit -> unit Js.meth
      method close_     : unit -> unit Js.meth
      method is_open_   : unit -> bool Js.t Js.meth
    end

  type events =
    { accept : Dom_html.event Js.t Dom_events.Typ.typ
    ; cancel : Dom_html.event Js.t Dom_events.Typ.typ
    }

  let events = { accept = Dom_events.Typ.make "MDCDialog:accept"
               ; cancel = Dom_events.Typ.make "MDCDialog:cancel"
               }

  let create ?id ?style ?classes ?attrs ~label_id ~description_id ~content () : t Js.t =
    let (elt : t Js.t) = create ?id ?style ?classes ?attrs ~label_id ~description_id ~content ()
                         |> Tyxml_js.To_dom.of_element
                         |> Js.Unsafe.coerce in
    let set = fun (x : t Js.t) (name : string) f -> Js.Unsafe.set x name f in
    set elt "component" @@ Js.Unsafe.(fun_call (js_expr "mdc.dialog.MDCDialog.attachTo") [| inject elt |]);
    elt

end

module Drawer = struct

  include Widgets.Drawer

end

module Elevation = struct

  include Widgets.Elevation

end

module Fab = struct

  include Widgets.Fab

  class type t =
    object
      inherit Dom_html.element
    end

  let create ?id ?style ?classes ?attrs ?mini ?plain ?ripple ?label ~icon () : t Js.t =
    create ?id ?style ?classes ?attrs ?mini ?plain ?ripple ?label ~icon ()
    |> Tyxml_js.To_dom.of_element

end

module Form_field = struct

  include Widgets.Form_field

  class type t =
    object
      inherit Dom_html.element
    end

  let create ?id ?style ?classes ?attrs ?align_end ~input ~label () : t Js.t =
    create ?id ?style ?classes ?attrs ?align_end ~input ~label ()
    |> Tyxml_js.To_dom.of_element

end

module Grid_list = struct

  include Widgets.Grid_list

  class type t =
    object
      inherit Dom_html.element
    end

  let create ?id ?style ?classes ?attrs
             ?ar ?one_px_gutter ?header_caption ?twoline ?icon_align ~tiles () : t Js.t =
    create ?id ?style ?classes ?attrs ?ar ?one_px_gutter ?header_caption ?twoline ?icon_align ~tiles ()
    |> Tyxml_js.To_dom.of_element

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
      inherit Dom_html.element
      method component_    : component Js.t Js.readonly_prop
      method is_on_        : unit -> bool Js.t Js.meth
      method set_on_       : bool Js.t -> unit Js.meth
      method is_disabled_  : unit -> bool Js.t Js.meth
      method set_disabled_ : bool Js.t -> unit Js.meth
    end

  class type detail =
    object
      method isOn : bool Js.t Js.readonly_prop
    end

  class type change_event =
    object
      inherit Dom_html.event
      method detail : detail Js.t Js.readonly_prop
    end

  type events =
    { change : change_event Js.t Dom_events.Typ.typ
    }

  let events =
    { change = Dom_events.Typ.make "MDCIconToggle:change"
    }

  let create ?id ?style ?classes ?attrs ?disabled
             ~on_content ?on_label ?on_class
             ~off_content ?off_label ?off_class
             () : t Js.t =
    let (elt : t Js.t) = create ?id ?style ?classes ?attrs ?disabled
                                ~on_content ?on_label ?on_class
                                ~off_content ?off_label ?off_class ()
                         |> Tyxml_js.To_dom.of_element
                         |> Js.Unsafe.coerce in
    let set = fun (x : t Js.t) (name : string) f -> Js.Unsafe.set x name f in
    set elt "component"    @@ Js.Unsafe.(fun_call (js_expr "mdc.iconToggle.MDCIconToggle.attachTo")
                                                  [| inject elt |]);
    set elt "is_on"        @@ Js.wrap_callback (fun () -> elt##.component_##.on_);
    set elt "set_on"       @@ Js.wrap_callback (fun x  -> elt##.component_##.on_ := x);
    set elt "is_disabled"  @@ Js.wrap_callback (fun () -> elt##.component_##.disabled_);
    set elt "set_disabled" @@ Js.wrap_callback (fun x  -> elt##.component_##.disabled_ := x);
    elt

end

module Layout_grid = struct

  include Widgets.Layout_grid

  class type t =
    object
      inherit Dom_html.element
    end

  let create ?id ?style ?classes ?attrs ?align ?fixed_column_width ~content () : t Js.t =
    create ?id ?style ?classes ?attrs ?align ?fixed_column_width ~content ()
    |> Tyxml_js.To_dom.of_element

end

module Linear_progress = struct

  include Widgets.Linear_progress

  class type component =
    object
      method determinate_ : bool Js.t Js.writeonly_prop
      method progress_    : Js.number Js.writeonly_prop
      method buffer_      : Js.number Js.writeonly_prop
      method reverse_     : bool Js.t Js.writeonly_prop
      method open_        : unit -> unit Js.meth
      method close_       : unit -> unit Js.meth
    end

  class type t =
    object
      inherit Dom_html.element
      method component_       : component Js.t Js.readonly_prop
      method set_determinate_ : bool Js.t -> unit Js.meth
      method set_progress_    : Js.number Js.t -> unit Js.meth
      method set_buffer_      : Js.number Js.t -> unit Js.meth
      method set_reverse_     : bool Js.t -> unit Js.meth
      method open_            : unit -> unit Js.meth
      method close_           : unit -> unit Js.meth
    end

  let create ?id ?style ?classes ?attrs
             ?buffering_dots_id ?buffering_dots_style ?buffering_dots_classes ?buffering_dots_attrs
             ?buffer_id ?buffer_style ?buffer_classes ?buffer_attrs
             ?primary_bar_id ?primary_bar_style ?primary_bar_classes ?primary_bar_attrs
             ?secondary_bar_id ?secondary_bar_style ?secondary_bar_classes ?secondary_bar_attrs
             ?indeterminate ?reversed ?accent () : t Js.t =
    let (elt : t Js.t) = create ?id ?style ?classes ?attrs
                                ?buffering_dots_id ?buffering_dots_style ?buffering_dots_classes ?buffering_dots_attrs
                                ?buffer_id ?buffer_style ?buffer_classes ?buffer_attrs
                                ?primary_bar_id ?primary_bar_style ?primary_bar_classes ?primary_bar_attrs
                                ?secondary_bar_id ?secondary_bar_style ?secondary_bar_classes ?secondary_bar_attrs
                                ?indeterminate ?reversed ?accent ()
                         |> Tyxml_js.To_dom.of_element
                         |> Js.Unsafe.coerce in
    let set = fun (x : t Js.t) (name : string) f -> Js.Unsafe.set x name f in
    set elt "component"       @@ Js.Unsafe.(fun_call (js_expr "mdc.linearProgress.MDCLinearProgress.attachTo")
                                                     [| inject elt |]);
    set elt "set_determinate" @@ Js.wrap_callback (fun x  -> elt##.component_##.determinate_ := x);
    set elt "set_progress"    @@ Js.wrap_callback (fun x  -> elt##.component_##.progress_ := x);
    set elt "set_buffer"      @@ Js.wrap_callback (fun x  -> elt##.component_##.buffer_ := x);
    set elt "set_reverse"     @@ Js.wrap_callback (fun x  -> elt##.component_##.reverse_ := x);
    set elt "open"            @@ Js.wrap_callback (fun () -> elt##.component_##open_ ());
    set elt "close"           @@ Js.wrap_callback (fun () -> elt##.component_##close_ ());
    elt

end

module List_ = struct

  include Widgets.List_

  class type t =
    object
      inherit Dom_html.element
    end

end

module Menu = struct

  include Widgets.Menu

  class type t =
    object
      inherit Dom_html.element
    end

end

module Radio = struct

  include Widgets.Radio

  class type component =
    object
      method checked_  : bool Js.t Js.prop
      method disabled_ : bool Js.t Js.prop
      method value_    : Js.js_string Js.prop
    end

  class type t =
    object
      inherit Dom_html.element
      method component_    : component Js.t Js.readonly_prop
      method is_checked_   : unit -> bool Js.t Js.meth
      method set_checked_  : bool Js.t -> unit Js.meth
      method is_disabled_  : unit -> bool Js.t Js.meth
      method set_disabled_ : bool Js.t -> unit Js.meth
      method get_value_    : unit -> Js.js_string Js.meth
      method set_value_    : Js.js_string -> unit Js.meth
    end

  let create ?id ?input_id ?style ?classes ?attrs ?js ?checked ?disabled ~name () : t Js.t =
    let (elt : t Js.t) = create ?id ?input_id ?style ?classes ?attrs ?js ?checked ?disabled ~name ()
                         |> Tyxml_js.To_dom.of_element
                         |> Js.Unsafe.coerce in
    let set = fun (x : t Js.t) (name : string) f -> Js.Unsafe.set x name f in
    set elt "component"    @@ Js.Unsafe.(fun_call (js_expr "mdc.radio.MDCRadio.attachTo") [| inject elt |]);
    set elt "is_checked"   @@ Js.wrap_callback (fun () -> elt##.component_##.checked_);
    set elt "set_checked"  @@ Js.wrap_callback (fun x  -> elt##.component_##.checked_ := x);
    set elt "is_disabled"  @@ Js.wrap_callback (fun () -> elt##.component_##.disabled_);
    set elt "set_disabled" @@ Js.wrap_callback (fun x  -> elt##.component_##.disabled_ := x);
    set elt "get_value"    @@ Js.wrap_callback (fun () -> elt##.component_##.value_);
    set elt "set_value"    @@ Js.wrap_callback (fun x  -> elt##.component_##.value_ := x);
    elt

end

module Ripple = struct

  include Widgets.Ripple

  class type t =
    object
      inherit Dom_html.element
    end

end

module Select = struct

  include Widgets.Select

  class type t =
    object
      inherit Dom_html.element
    end

end

module Slider = struct

  include Widgets.Slider

  class type t =
    object
      inherit Dom_html.element
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

  class type component =
    object
      method show_              : (string * Js.Unsafe.any) array -> unit Js.meth
      method dismissesOnAction_ : bool Js.t Js.prop
    end

  class type t =
    object
      inherit Dom_html.element
      method component_               : component Js.t Js.readonly_prop
      method show_                    : data -> unit Js.meth
      method is_dismisses_on_action_  : unit -> bool Js.t Js.meth
      method set_dismisses_on_action_ : bool Js.t -> unit Js.meth
    end

  let create ?id ?style ?classes ?attrs ?start_aligned ?(dismisses_on_action=true) () : t Js.t =
    let (>|=) x f = Js.Optdef.map x f in
    let (elt : t Js.t) = create ?id ?style ?classes ?attrs ?start_aligned ()
                         |> Tyxml_js.To_dom.of_element
                         |> Js.Unsafe.coerce in
    let set = fun (x : t Js.t) (name : string) f -> Js.Unsafe.set x name f in
    set elt "component" @@ Js.Unsafe.(fun_call (js_expr "mdc.snackbar.MDCSnackbar.attachTo") [| inject elt |]);
    elt##.component_##.dismissesOnAction_ := Js.bool dismisses_on_action;
    set elt "show" @@ Js.wrap_callback (fun x ->
                          let wrap = fun x f -> Js.Optdef.option x >|= f |> Js.Unsafe.inject in
                          let data = [| "message",       Js.Unsafe.inject x.message
                                      ; "timeout",       wrap x.timeout (fun x -> x)
                                      ; "actionHandler", wrap x.action (fun x -> Js.wrap_callback x.handler)
                                      ; "actionText",    wrap x.action (fun x -> Js.string x.text)
                                      ; "multiline",     wrap x.multiline (fun x -> Js.bool x.enable)
                                      ; "actionOnBottom",wrap x.multiline (fun x -> Js.bool x.action_on_bottom)
                                     |] |> Js.Unsafe.obj in
                          elt##.component_##show_ data);
    set elt "is_dismisses_on_action"  @@ Js.wrap_callback (fun () -> elt##.component_##.dismissesOnAction_);
    set elt "set_dismisses_on_action" @@ Js.wrap_callback (fun x  -> elt##.component_##.dismissesOnAction_ := x);
    elt

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

  let create ?id ?input_id ?style ?classes ?attrs ?disabled () : t Js.t =
    let (elt : t Js.t) = create ?id ?input_id ?style ?classes ?attrs ?disabled ()
                         |> Tyxml_js.To_dom.of_element
                         |> Js.Unsafe.coerce in
    let set    = fun (x : t Js.t) (name : string) f -> Js.Unsafe.set x name f in
    let get_nc = fun () -> elt##querySelector (Js.string ("." ^ native_control_class)) in
    set elt "is_checked"   @@ Js.wrap_callback (fun () -> match Js.Opt.to_option @@ get_nc () with
                                                          | Some nc -> (Js.Unsafe.coerce nc)##.checked
                                                          | None    -> Js._false);
    set elt "set_checked"  @@ Js.wrap_callback (fun x -> match Js.Opt.to_option @@ get_nc () with
                                                         | Some nc -> (Js.Unsafe.coerce nc)##.checked := x
                                                         | None    -> ());
    set elt "is_disabled"  @@ Js.wrap_callback (fun () -> match Js.Opt.to_option @@ get_nc () with
                                                          | Some nc -> (Js.Unsafe.coerce nc)##.disabled
                                                          | None    -> Js._false);
    set elt "set_disabled" @@ Js.wrap_callback (fun x -> match Js.Opt.to_option @@ get_nc () with
                                                         | Some nc -> (Js.Unsafe.coerce nc)##.disabled := x
                                                         | None    -> ());
    set elt "get_value"    @@ Js.wrap_callback (fun () -> match Js.Opt.to_option @@ get_nc () with
                                                          | Some nc -> (Js.Unsafe.coerce nc)##.value
                                                          | None    -> Js.string "");
    set elt "set_value"    @@ Js.wrap_callback (fun x -> match Js.Opt.to_option @@ get_nc () with
                                                         | Some nc -> (Js.Unsafe.coerce nc)##.value := x
                                                         | None    -> ());
    elt


end

module Tabs = struct

  include Widgets.Tabs

  class type t =
    object
      inherit Dom_html.element
    end

end

module Textfield = struct

  include Widgets.Textfield

  class type t =
    object
      inherit Dom_html.element
    end

end

module Toolbar = struct

  include Widgets.Toolbar

  class type t =
    object
      inherit Dom_html.element
    end

end

module Typography = struct

  include Widgets.Typography

end
