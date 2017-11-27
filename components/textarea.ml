module Pure = struct

  class t ?input_id ?placeholder ?rows ?cols () =

    let elt = Markup.Textfield.create ?input_id ?placeholder ?rows ?cols ~textarea:true ()
              |> Tyxml_js.To_dom.of_div in

    object

      inherit Widget.text_input_widget elt ()

      val input_element : Dom_html.inputElement Js.t =
        elt##querySelector (Js.string ("." ^ Markup.Textfield.input_class))
        |> Js.Opt.to_option |> CCOpt.get_exn |> Js.Unsafe.coerce

      method input_element = input_element

    end

end

class type mdc =
  object
    method disabled : bool Js.t Js.prop
    method valid    : bool Js.t Js.writeonly_prop
  end

class t ?input_id ?label ?placeholder ?rows ?cols () =

  let elt = (Markup.Textfield.create ?input_id ?label ?placeholder ?rows ?cols ~textarea:true ()
             |> Tyxml_js.To_dom.of_div) in

  object(self)

    inherit Widget.input_widget elt ()

    val mdc : mdc Js.t = elt |> (fun x -> Js.Unsafe.global##.mdc##.textField##.MDCTextField##attachTo x)

    val input_element : Dom_html.inputElement Js.t =
      elt##querySelector (Js.string ("." ^ Markup.Textfield.input_class))
      |> Js.Opt.to_option |> CCOpt.get_exn |> Js.Unsafe.coerce

    method input_element = input_element

    method dense          = self#add_class Markup.Textfield.dense_class
    method full_width     = self#add_class Markup.Textfield.fullwidth_class
    method not_dense      = self#remove_class Markup.Textfield.dense_class
    method not_full_width = self#remove_class Markup.Textfield.fullwidth_class

    method disable         = mdc##.disabled := Js._true
    method enable          = mdc##.disabled := Js._false
    method toggle_disabled = mdc##.disabled := Js.bool @@ not self#disabled

    method valid   = mdc##.valid := Js._true
    method invalid = mdc##.valid := Js._false

  end
