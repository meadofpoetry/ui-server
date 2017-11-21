open Widget
open Markup
open Tyxml_js

class type mdc =
  object
    method checked       : bool Js.t Js.prop
    method indeterminate : bool Js.t Js.prop
    method disabled      : bool Js.t Js.prop
    method value         : Js.js_string Js.t Js.prop
  end

class t ?input_id () =

  let elt = Checkbox.create ?input_id () |> To_dom.of_div in

  object(self)

    inherit [Dom_html.divElement Js.t] radio_or_cb_widget elt ()

    val mdc : mdc Js.t = Js.Unsafe.global##.mdc##.checkbox##.MDCCheckbox##attachTo elt

    val input : Dom_html.inputElement Js.t =
      elt##querySelector (Js.string ("." ^ Checkbox.native_control_class))
      |> Js.Opt.to_option |> CCOpt.get_exn |> Js.Unsafe.coerce

    method private input = input

    method indeterminate        = Js.to_bool (Js.Unsafe.coerce input)##.indeterminate
    method set_indeterminate    = (Js.Unsafe.coerce input)##.indeterminate := Js._true
    method set_determinate      = (Js.Unsafe.coerce input)##.indeterminate := Js._false
    method toggle_indeterminate = (Js.Unsafe.coerce input)##.indeterminate := Js.bool @@ not self#indeterminate

  end
