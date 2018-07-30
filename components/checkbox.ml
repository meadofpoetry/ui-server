open Containers
open Tyxml_js

module Markup = Components_markup.Checkbox.Make(Xml)(Svg)(Html)

class t ?(ripple=true) ?state ?on_change ?input_id () =

  let elt       = Markup.create ?input_id () |> Tyxml_js.To_dom.of_div in
  let input_elt = elt##querySelector (Js.string ("." ^ Markup.native_control_class))
                  |> Js.Opt.to_option |> Option.get_exn |> Js.Unsafe.coerce in

  object
    inherit Widget.radio_or_cb_widget ?state ?on_change ~input_elt elt ()
    method set_indeterminate x = (Js.Unsafe.coerce input_elt)##.indeterminate := Js.bool x
    method indeterminate       = Js.to_bool (Js.Unsafe.coerce input_elt)##.indeterminate
    initializer
      if ripple then Js.Unsafe.global##.mdc##.checkbox##.MDCCheckbox##attachTo elt
  end
