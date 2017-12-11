class t ?input_id () =
  let elt = Markup.Switch.create ?input_id () |> Tyxml_js.To_dom.of_div in
  let input_elt = elt##querySelector (Js.string ("." ^ Markup.Switch.native_control_class))
                  |> Js.Opt.to_option |> CCOpt.get_exn |> Js.Unsafe.coerce in
  object
    inherit Widget.radio_or_cb_widget ~input_elt elt ()
  end
