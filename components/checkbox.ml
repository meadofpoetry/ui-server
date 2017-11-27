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

class t ?(ripple=true) ?input_id () =

  let elt = Checkbox.create ?input_id () |> To_dom.of_div in

  object(self)

    inherit radio_or_cb_widget elt ()

    val input_element : Dom_html.inputElement Js.t =
      elt##querySelector (Js.string ("." ^ Checkbox.native_control_class))
      |> Js.Opt.to_option |> CCOpt.get_exn |> Js.Unsafe.coerce

    method input_element = input_element

    method indeterminate        = Js.to_bool (Js.Unsafe.coerce self#input_element)##.indeterminate
    method set_indeterminate    = (Js.Unsafe.coerce self#input_element)##.indeterminate := Js._true
    method set_determinate      = (Js.Unsafe.coerce self#input_element)##.indeterminate := Js._false
    method toggle_indeterminate = (Js.Unsafe.coerce self#input_element)##.indeterminate
                                  := Js.bool @@ not self#indeterminate

    initializer
      if ripple then Js.Unsafe.global##.mdc##.checkbox##.MDCCheckbox##attachTo elt

  end
