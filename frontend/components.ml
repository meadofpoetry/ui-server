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

  let create ?id ?style ?classes ?attrs ?align_end ~input ~label () =
    create ?id ?style ?classes ?attrs ?align_end ~input ~label ()
    |> Tyxml_js.To_dom.of_element

end

module Grid_list = struct

  include Widgets.Grid_list

end

module Icon_toggle = struct

  include Widgets.Icon_toggle

end

module Layout_grid = struct

  include Widgets.Layout_grid

end

module Linear_progress = struct

  include Widgets.Linear_progress

end

module List_ = struct

  include Widgets.List_

end

module Menu = struct

  include Widgets.Menu

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

end

module Select = struct

  include Widgets.Select

end

module Slider = struct

  include Widgets.Slider

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

end

module Textfield = struct

  include Widgets.Textfield

end

module Toolbar = struct

  include Widgets.Toolbar

end

module Typography = struct

  include Widgets.Typography

end
