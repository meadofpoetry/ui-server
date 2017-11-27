class type mdc =
  object
    method checked  : bool Js.t Js.prop
    method disabled : bool Js.t Js.prop
    method value    : Js.js_string Js.t Js.prop
  end

class t ?(ripple=true) ?input_id ~name () =

  let elt = Markup.Radio.create ?input_id ~name () |> Tyxml_js.To_dom.of_i in

  object
    inherit Widget.radio_or_cb_widget elt ()

    val input_element : Dom_html.inputElement Js.t =
      elt##querySelector (Js.string ("." ^ Markup.Radio.native_control_class))
      |> Js.Opt.to_option |> CCOpt.get_exn |> Js.Unsafe.coerce

    method input_element = input_element

    initializer
      if ripple then Js.Unsafe.global##.mdc##.radio##.MDCRadio##attachTo elt
  end
