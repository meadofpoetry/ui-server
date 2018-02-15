open Containers

class t ?(ripple=true) ?input_id () =

  let elt       = Markup.Checkbox.create ?input_id () |> Tyxml_js.To_dom.of_div in
  let input_elt = elt##querySelector (Js.string ("." ^ Markup.Checkbox.native_control_class))
                  |> Js.Opt.to_option |> Option.get_exn |> Js.Unsafe.coerce in

  object
    inherit Widget.radio_or_cb_widget ~input_elt elt ()
    method set_indeterminate x = (Js.Unsafe.coerce input_elt)##.indeterminate := Js.bool x
    method get_indeterminate   = Js.to_bool (Js.Unsafe.coerce input_elt)##.indeterminate
    initializer
      if ripple then Js.Unsafe.global##.mdc##.checkbox##.MDCCheckbox##attachTo elt
  end
