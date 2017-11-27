class t ?input_id () =

  let elt = Markup.Switch.create ?input_id () |> Tyxml_js.To_dom.of_div in

  object
    inherit Widget.radio_or_cb_widget elt ()

    val input_element = elt##querySelector (Js.string ("." ^ Markup.Switch.native_control_class))
                        |> Js.Opt.to_option |> CCOpt.get_exn |> Js.Unsafe.coerce

    method input_element = input_element
  end
